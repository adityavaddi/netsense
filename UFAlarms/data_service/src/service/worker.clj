(ns service.worker
  (:require [zeromq.zmq :as zmq]
            [clojure.data.json :as json]
            [clojure.core.match :refer [match]]
            [adapters.core]
            [common.core]
            [service.datasrc]
            [service.stubbeddatasrc]
            [service.graphdatasrc]
            [adapters.cassandra]
            [adapters.elasticsearch]
            [msgpack.core :as msgpk])
  (:import (java.io StringWriter PrintWriter)))

(defn- to-json [msg]
  ;; NOTE the following relies on [expensive] exception throwing to de-multiplex
  ;; plaintext JSON and msgpack'd JSON. Replace with separate sockets when perf
  ;; becomes an issue.
  (let [msg-str (try (new String msg "UTF-8") (catch Exception e msg))
        try-read-json (fn [msg] (try (json/read-str msg :key-fn keyword) (catch Exception _ false)))
        maybe-json (try-read-json msg-str)]
    (match [maybe-json]
      [false] (let [unpacked (msgpk/unpack msg)] [unpacked (try-read-json unpacked)])
      [_] [false maybe-json])))

(def ^:private config (delay (common.core/get-config)))

(try (def ^:private consumers (delay [
  (adapters.cassandra/CassandraEventConsumer (get-in @config [:databases :cassandra :ip]))
  (adapters.elasticsearch/ElasticsearchEventConsumer (get-in @config [:databases :elasticsearch :ip]))]))
(catch Exception e (do
  (.println *err* (format "Error instantiating consumers: %s" (.getMessage e)))
  (System/exit 1))))

(defn- get-stack-trace [excp]
  (let [trace (new StringWriter)]
    (do (.printStackTrace excp (new PrintWriter trace)) (.toString trace))))

(defn- listen-on [port lup]
  (let [context (zmq/zcontext)]
    (with-open [responder (doto (zmq/socket context :rep)
                            (zmq/connect (format "tcp://127.0.0.1:%s" port)))]
      (while true (lup responder)))))

(defn- backend-listen-on [port]
  (listen-on port (fn [responder]
    (let [msg (zmq/receive responder)
          [unpacked json] (to-json msg)
          print-and-send-response (fn [msg resp] (do (println msg) (flush) (zmq/send responder resp)))]
      (match [unpacked json]
        [_ false] (print-and-send-response
          (format "Invalid JSON! msg: '%s', unpacked: '%s'" msg unpacked)
          (msgpk/pack "ERROR: unable to unpack and/or parse JSON."))
        [_ _] (print-and-send-response
          (format "Received request: '%s'" (json/write-str json))
          (let [log-event (adapters.core/map->LogEvent json)
                reply (clojure.string/join "||__||" (doall (pmap (fn [consumer]
                  (try
                    (adapters.core/consume-log-event consumer log-event)
                  (catch Exception e
                    (format "Error inserting consumer: '%s', message: '%s': stack: '%s'"
                      consumer (.getMessage e) (get-stack-trace e))))) @consumers)))]
                (if unpacked (msgpk/pack reply) (.getBytes reply)))))))))

(defn- handle-query [uid query-type & params]
  (json/write-str (match [query-type]
    ["GET"] (apply (partial service.datasrc/handle-select uid) params)
    ["CREATE"] (apply (partial service.datasrc/handle-insert uid) params)
    ["UPDATE"] (apply (partial service.datasrc/handle-update uid) params)
    ["DELETE"] (apply (partial service.datasrc/handle-delete uid) params))))

(defn- frontend-listen-on [port]
  (listen-on port (fn [responder]
    (let [raw (zmq/receive responder)
          msg (String. raw)
          [uid query params] (clojure.string/split msg #":")
          res (try (if (= query "$SCHEMA$")
                ((partial service.datasrc/handle-schema-batch uid) params)
                (apply (partial handle-query uid) (clojure.string/split query #"\|")))
              (catch AssertionError e (do (println (format "ERROR: %s" (get-stack-trace e))) nil)))]
      (println (format "Returning for uid: %s: %s" uid res))
      (zmq/send responder (.getBytes (format "%s|%s" uid res)))))))

(defn -main [& args]
  (let [stub-data? (contains? (set args) "--stubdata")]
    (service.datasrc/set-data-source (if stub-data? service.stubbeddatasrc/StubbedDataSource
                                       service.graphdatasrc/GraphDataSource))
    (let [msgpack-ports (get-in @config [:listen-ports :backend :msgpack])
          plaintxt-ports (get-in @config [:listen-ports :backend :plaintxt])
          frontend-ports (get-in @config [:listen-ports :frontend])
          port-pairs (common.core/map-ports msgpack-ports plaintxt-ports frontend-ports)
          [backend-port-pairs frontend-port-pairs] (partition-all (- (count port-pairs) (count frontend-ports)) port-pairs)
          [backend-listen-ports frontend-listen-ports] [(map second backend-port-pairs) (map second frontend-port-pairs)]
          create-listeners (fn [listener ports] (doall (map (fn [port] (future (listener port))) ports)))
          backend-futures (create-listeners backend-listen-on backend-listen-ports)
          frontend-futures (create-listeners frontend-listen-on frontend-listen-ports)
          futures (concat backend-futures frontend-futures)]
    (println (format "Service.Worker started%s. Listening on backend ports: %s, frontend ports: %s"
       (if stub-data? " (serving stub data)" "") (vec backend-listen-ports) (vec frontend-listen-ports)))
    (let [fut (common.core/future-sequence futures)] @fut))))
