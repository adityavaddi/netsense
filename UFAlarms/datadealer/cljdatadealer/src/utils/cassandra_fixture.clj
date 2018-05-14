(ns utils.cassandra_fixture (:gen-class)
  (:refer-clojure :exclude [update])
  (:use clojure.tools.logging)
  (:require [utils.config :as conf]
            [utils.cassandra-cql :as cql]
            [utils.cassandra_init :as init]
            [utils.cassandra_intf :as cass
             :refer [conn]]))

(defn cassandra-fixture
  [f]
  (let [config (conf/cassservice)
        keyspace (:keyspace config)
        strategy (:class config)
        factor (:factor config)]
    (try
      (cql/drop-keyspace @conn keyspace)
      (catch Exception _ nil))
    (cql/create_keyspaces @conn keyspace strategy factor)
    (init/initialize_tables)
    (init/load_tables keyspace)
    (f)))
