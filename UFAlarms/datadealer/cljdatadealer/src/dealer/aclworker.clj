(ns ^{:doc "Acl Worker."}
  dealer.aclworker
  (:gen-class)
  (:require [acl.core :as acl]
            [clojure.data.json :as json]
            [clojure.tools.logging :refer :all]
            [clojure.core.async.impl.concurrent :as conc]
            [clojure.walk :refer :all]
            [com.stuartsierra.component :as component]
            [dealer [metrics :as metrics]]
            [kafka [streams :as streams]]
            [metrics
             [counters :as counters]
             [gauges :as gauge]
             [histograms :as histograms]
             [meters :as meters]
             [timers :as timers]])
  (:import java.util.concurrent.Executors
           [org.apache.kafka.streams.kstream KStreamBuilder ValueMapper]))



(defonce metric-factory
  (dealer.metrics/metric-factory-stub "dealer.aclworker"))

(def acl-timer-name "aclworker")

(defn query-exec-json [jsmsg]
  (let [jsmap (json/read-str jsmsg)
        messageid (get jsmap "messageid")
        msg (->
              jsmap
              (get "request")
              (keywordize-keys))]
    (try
      (debugf "ACL service request: %s" jsmsg)
      (debugf "ACL service jsmap %s" msg)

      (let [result (acl/actrl msg)]
        (debug (str "Result: " result)) (flush)
        (json/write-str {:messageid messageid :response result}))
      (catch clojure.lang.ExceptionInfo e
        (error (format "caught exception: %s %s" (.getMessage e) (ex-data e)))
        (json/write-str {:messageid messageid :response {:error (.getMessage e) :success false :status (get (ex-data e) :status)}}))
      (catch Exception e
        (error (format "caught exception: %s" (.getMessage e)))
        (json/write-str {:messageid messageid :response {:error (.getMessage e) :success false :status 500}})))))

(defn connect [config n m]
  (let [stream-config (streams/streams-config (:kafka config))
        stream-builder (streams/streams-builder)
        in-topic (into-array String [(get-in config [:zookeeper :topic])])
        out-topic (get-in config [:kafka :topic])
        handler (reify Thread$UncaughtExceptionHandler
                  (uncaughtException [_ thread ex]
                    (error ex "Uncaught exception in kafka stream on" (.getName thread))
                    (errorf "Sleeping %d, then retrying." n)
                    (Thread/sleep n)
                    (connect config (min (* 2 n) m) m)))]
    (->
      stream-builder
      (.stream in-topic)
      (.mapValues (reify ValueMapper (apply [_ value]
                                       (query-exec-json value))))
      (.to out-topic))
    (streams/start stream-builder stream-config handler)))

(defrecord AclWorker [config
                          kafka-connection
                          metrics-registry]
  component/Lifecycle
  (start [component]
    (info "Starting ACL Worker.")
    (connect config 250 5000)
    component)

  (stop [component]
    (info "Stopping ACL Worker.")
    component))

(defn new-aclworker
  [config]
  (map->AclWorker {:config config}))
