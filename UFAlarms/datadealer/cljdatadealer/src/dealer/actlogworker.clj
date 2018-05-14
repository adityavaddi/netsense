(ns ^{:doc "The Activity Log Worker."}
    dealer.actlogworker
  (:gen-class)
  (:require [clojure.data.json :as json]
            [clojure.tools.logging :refer :all]
            [com.stuartsierra.component :as component]
            [dealer.worker :as worker]
            [msgpack.core :as msgpk]
            [utils
             [cape :as qb]
             [config :as conf]]))

(def zmqconf (conf/zmqservice))

;; Activity log message format
;; msg: {user: <userid>, type: 'activityLog', filter: <filter>}
(defn query-exec-json [jsmsg]
  (let [jsmap (json/read-str jsmsg)
        messageid (get jsmap "messageid")]
    (try
      (let [result (qb/executer-main (get jsmap "request"))]
        ;;(debug (str "Result: " result)) (flush)
        (json/write-str {:messageid messageid, :response (json/read-str result)}))
      (catch clojure.lang.ExceptionInfo e
        (error (format "caught exception: %s %s" (.getMessage e) (ex-data e)))
        (json/write-str {:messageid messageid, :response {:error (.getMessage e) :success false :status (get (ex-data e) :status)}}))
      (catch Exception e
        (error (format "caught exception: %s" (.getMessage e)))
        (json/write-str {:messageid messageid, :response {:error (.getMessage e) :success false :status 500}})))))

(defn startworker []
  (let [port (:be (:js (:actlog zmqconf)))
        broker (:broker (:actlog zmqconf))
        dealersocket (str "tcp://" broker ":" port)]
    (info "Activity Logger.Worker started....")
    (dotimes [n 40]
      (worker/startasyncworker dealersocket "json" query-exec-json))))

(defrecord ActivityLogWorker []
  component/Lifecycle
  (start [component]
    (startworker)
    component)
  (stop [component]
    (info "Stopping Activity Log Worker.")))

(defn new-activity-log-worker
  []
  (map->ActivityLogWorker {}))
