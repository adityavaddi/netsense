(ns ^{:doc "The Kafka Device Service Worker."}
      dealer.devsvcworker-kafka
  (:gen-class)
  (:require [clojure.data.json :as json]
            [clojure.tools.logging :refer :all]
            [clojure.walk :refer :all]
            [dealer.devsvcworker :as worker]
            [com.stuartsierra.component :as component])
  (:import java.util.concurrent.Executors))

(defn map-sensor-fields [m]
  (let [r (:l m)]
    {:name "SensorSample" :nodeid (:n r) :sensor (:s r) :value (:v r) :time (:t r)}))

(defrecord DeviceServiceWorkerKafka [config kafka-sensor-connector]
  component/Lifecycle
  (start [component]
    (info "Starting Kafka Device Service Worker..")
    (.start
      (Thread.
        (fn []
          (let [{:keys [connection]} kafka-sensor-connector]
            (loop []
              (let [records (.poll connection 1000)]
                (dorun (map (fn [record]
                              (let [kw-map (-> record
                                               (.value)
                                               (java.lang.String.)
                                               (json/read-str :key-fn keyword)
                                               (map-sensor-fields))]
                                (worker/query-exec-msgpk kw-map)))
                            records))
                (.commitAsync connection))
              (recur))))))
    (info "Started Kafka Device Service Worker.")
    component)

  (stop [component]
    (info "Stopping Kafka Device Service Worker.")
    component))

(defn new-device-service-worker-kafka
  [config]
  (map->DeviceServiceWorkerKafka {:config config}))

