(ns utils.fixtures
  (:gen-class)
  (:require [dealer.core :as core]
            [mqtt.mock :as mock-mqtt]
            [dealer.devsvcctrl-test :as dev-ctrl-test]
            [utils
             [cassandra_fixture :as cass]
             [neo4j-fixture :as neo]]))

(defn -main [& _]
  (cass/cassandra-fixture
   #(neo/neo4j-fixture
     core/-main)))
