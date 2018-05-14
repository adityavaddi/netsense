(ns ^{:doc "Cape Worker."}
  dealer.capeworker
  (:gen-class)
  (:require [clojure.data.json :as json]
            [clojure.tools.logging :refer :all]
            [clojure.core.async.impl.concurrent :as conc]
            [clojure.walk :refer :all]
            [com.stuartsierra.component :as component]
            [dealer [metrics :as metrics]]
            [utils [cape :as cape]]
            [kafka [capestreams :as capestreams]]
            [metrics
             [counters :as counters]
             [gauges :as gauge]
             [histograms :as histograms]
             [meters :as meters]
             [timers :as timers]]
            [utils [config :as config]])
  (:import java.util.concurrent.Executors
           [org.apache.kafka.streams.kstream KStreamBuilder ValueMapper]))

(defonce metric-factory
  (dealer.metrics/metric-factory-stub "dealer.capeworker"))

(def cape-timer-name "capeworker")

(defn executeCape
  ; expects the resolved message
  [msg]
    ; Process the message in this thread
    (spy :debug (cape/executer-device msg))
  )

(defn query-exec-json [jsmsg]
  (let [{msg :request
         :keys [messageid]
         :as jsmap} (json/read-str jsmsg :key-fn keyword)]
    (try
      (debugf "Cape service request: %s" jsmsg)
      (debugf "Cape service jsmap %s" msg)

      (let [result (executeCape msg)]
        (debug (str "Result: " result)) (flush)
        (json/write-str {:messageid messageid :response result}))
      (catch clojure.lang.ExceptionInfo e
        (error e)
        (json/write-str {:messageid messageid :response {:error (.getMessage e) :success false :status (get (ex-data e) :status)}}))
      (catch Exception e
        (error (format "caught exception: %s" (.getMessage e)))
        (json/write-str {:messageid messageid :response {:error (.getMessage e) :success false :status 500}})))))

(defn connect [config n m]
  (let [stream-config (capestreams/cape-streams-config (:kafka config))
        stream-builder (capestreams/cape-streams-builder)
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
    (capestreams/start stream-builder stream-config handler)))

(defrecord CapeWorker [config]
  component/Lifecycle
  (start [component]
    (info "Starting Cape Worker.")
    (connect config 250 5000)
    component)

  (stop [component]
    (info "Stopping Cape Worker.")
    component))

(defn new-capeworker
  [config]
  (map->CapeWorker {:config config} ))
