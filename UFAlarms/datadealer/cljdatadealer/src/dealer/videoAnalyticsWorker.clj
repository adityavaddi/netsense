(ns ^{:doc "Video Analytics Worker."}
  dealer.videoAnalyticsWorker
  (:gen-class)
  (:require [dealer.devsvcworker :as dev-svc-worker]
            [clojure.tools.logging :refer :all]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [msgpack.core :as msgpk]
            [clojure.walk :refer :all]
            [com.stuartsierra.component :as component])
  (:import [org.apache.kafka.clients.consumer ConsumerRecords ]))


(def msg-names
  {"ndpark" "NonDemarcatedParking"
   "dpark" "DemarcatedParking"
   "linec" "LineCrossing"
   "LineCrossing" "LineCrossing"
   "NonDemarcatedParking" "NonDemarcatedParking"
   "DemarcatedParking" "DemarcatedParking"
   "ParkingKeepOut" "ParkingKeepOut"
   "ObjectEntered" "ObjectEntering"
   "ObjectExit" "ObjectExiting"
   "pko" "ParkingKeepOut"
   "objent" "ObjectEntering"
   "objlev" "ObjectLeaving"
   "inventory" "Inventory"
   "batchtype" "BatchType"
   "batchconfig" "BatchConfig"
    "objdwl" "ObjectDwell"})

(defn parse-key [record]
  (if (nil? (.key record)) "" (String. (.key record))))

(defn parse-topic [key]
  (if (nil? key)
    {:nodeid "" :name "NOTFOUND"}
    (let [tokens (string/split key #"/")
          msg-type (nth tokens (- (count tokens) 1))
          evt-type (nth tokens, 4)
          nodeid (nth tokens, 1)
          name-prefix (get msg-names msg-type  "NOTFOUND")
          name-type (if (.startsWith evt-type "evt") "Event" "Config")
          name (str name-prefix name-type)]
      {:nodeid nodeid :name name})))

(defrecord VideoAnalyticsWorker [kafka-connector]
  component/Lifecycle
  (start [component]
    (info "Starting videoAnalyticsWorker.")
         (.start
           (Thread.
             (fn []
               (let [{:keys [connection]} kafka-connector]
                 (loop []
                   (debugf "Start processing...")
                   (let [records (.poll connection 1000)
                         result (doall (map (fn [record]
                                              (let [key (spy :debug (parse-key record))
                                                    mpmsg (.value record)
                                                    msg (keywordize-keys (msgpk/unpack mpmsg))]
                                                (dev-svc-worker/query-exec-msgpk (spy :debug (merge (parse-topic key) msg)))
                                                ;(.commitAsync connection)
                                                )) records))]
                     (debugf "Processed %d records" (count result))
                     (.commitAsync connection))
                   (recur))))))
    (info "Started videoAnalyticsWorker.")
    component)

  (stop [component]
    (info "Stopping videoAnalyticsWorker.")
    component))

(defn new-videoAnalyticsWorker []
  (map->VideoAnalyticsWorker {}))
