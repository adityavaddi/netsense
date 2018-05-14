(ns utils.fixtures
  (:gen-class)
  (:require [dealer.core :as core]
            [mqtt.mock :as mock-mqtt]
            [dealer.devsvcctrl-test :as dev-ctrl-test]
            [utils
             [cassandra_fixture :as cass]
             [neo4j-fixture :as neo]]))

(defn -main [& _]
  (alter-var-root #'core/system
                  (fn [system]
                    (-> system
                        deref
                        (dissoc :schedule-updater
                                :dh-scheduler
                                :pd-scheduler
                                :mqtt-listener )
                        (assoc :mqtt (mock-mqtt/new-mock-mqtt)
                               :mock-device-service-control (dev-ctrl-test/new-mock-device-service-control)))))
  (cass/cassandra-fixture
   #(neo/neo4j-fixture
     core/-main)))
