(ns ^{:doc "The Main Worker."}
    dealer.worker
  (:gen-class)
  (:require [clojure.stacktrace :as trace]
            [clojure.tools.logging :refer :all]
            [com.stuartsierra.component :as component]
            [dealer.dealer :as dealer]
            [metrics
             [counters :refer [counter dec! inc!]]
             [timers :as timers :refer [start stop]]]
            [msgpack.core :as msgpk]
            [utils.config :as conf]
            [zeromq.zmq :as zmq])
  (:import [org.zeromq ZFrame ZMsg]))

(def polling-zmq-name "worker-polling-zmq-sockets")
(def polling-zmq
  (counter polling-zmq-name))

(def zmq-timer-name "zmq-timer")
(defonce zmq-timer (timers/timer zmq-timer-name))

(def zmqconf (conf/zmqservice))

(defn handle-message [poller responder enc func]
  (cond (zmq/check-poller poller 0 :pollin)
        (when-let [msg (ZMsg/recvMsg responder)]
          ;(debugf "Received message %s size %s first %s" msg (.size msg) (.peekFirst msg))
          (case (.size msg)
            1 (when-let [heartbeat (.getFirst msg)]
                (when (= heartbeat dealer/HEARTBEAT)
                  (debugf "Received heartbeat from dealer..."))
                (.destroy msg))
            2 (let [third (.removeLast msg)
                    data (spyf :debug "Received request: [%s]"
                               (get {"json" (.toString third)
                                     "msgpack" (.getData third)} enc))
                    result (func data)]
                (.addLast msg result)
                (.send msg responder))

            ; TODO Implement liveness http://zguide.zeromq.org/java:ppworker
            ))))
(defn startasyncworker [dealersocket enc func]
  (let [context (zmq/zcontext)
        poller (zmq/poller context 2)]
    (.start (Thread. (fn []
                       (with-open [responder (doto (zmq/socket context :dealer)
                                               (zmq/connect dealersocket))]
                         (zmq/register poller responder :pollin)
                         (inc! polling-zmq)
                         (try
                           (while (not= -1 (zmq/poll poller 200))
                             (dec! polling-zmq)
                             (let [timer (start zmq-timer)]
                               (try
                                 (handle-message poller responder enc func)
                                 (finally (stop timer))))
                             (inc! polling-zmq))
                           (catch java.lang.InterruptedException e
                             (info e)
                             (info "Terminating asyncworker"))
                           (catch Exception e
                             (error e))
                           (finally (dec! polling-zmq)))))))))


(defn handle-receive [poller responder enc func]
  (cond (zmq/check-poller poller 0 :pollin)
        (when-let [msg (ZMsg/recvMsg responder)]
          ;(debugf "Received message %s size %s first %s" msg (.size msg) (.peekFirst msg))
          (when-let [first (.getFirst msg)]
            (if (= first dealer/HEARTBEAT)
              (debugf "Received heartbeat from dealer...")
              (let [data (spyf :debug "Received request: [%s]"
                               (get {"json" (.toString first)
                                     "msgpack" (.getData first)} enc))
                    result (func data)]
                (.destroy msg)))))))

(defn startasyncpullworker [dealersocket enc func]
  (let [context (zmq/zcontext)
        poller (zmq/poller context 2)]
    (.start (Thread. (fn []
                       (with-open [responder (doto (zmq/socket context :dealer)
                                               (zmq/bind dealersocket))]
                         (debugf "Listening on %s..." dealersocket)
                         (zmq/register poller responder :pollin)
                         (inc! polling-zmq)
                         (try
                           (while (not= -1 (zmq/poll poller 200))
                             (dec! polling-zmq)
                             (let [timer (start zmq-timer)]
                               (try
                                 (handle-receive poller responder enc func)
                                 (finally (stop timer))))
                             (inc! polling-zmq))
                           (catch java.lang.InterruptedException e
                             (info e)
                             (info "Terminating asyncproxy"))
                           (catch Exception e
                             (error e))
                           (finally (dec! polling-zmq)))))))))

(defrecord Worker [metrics-registry]
  component/Lifecycle
  (start [component]
    (let [{:keys [registry
                  prefix]} metrics-registry]
      (alter-var-root #'polling-zmq
                      (fn [& _]
                        (counter registry
                                 (conj prefix polling-zmq-name))))
      (alter-var-root #'zmq-timer
                      (fn [& _]
                        (timers/timer registry
                                      (conj prefix zmq-timer-name))))))
  (stop [component]
    component))

(defn new-worker
  []
  (map->Worker {}))
