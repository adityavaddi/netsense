(ns adapters.cassandra
  (:require [clojurewerkz.cassaforte.client :as cc]
            [clojurewerkz.cassaforte.cql :as cql]
            [adapters.core]))

(defn CassandraEventConsumer [ip]
  (let [conn (cc/connect [ip])]
    (cql/use-keyspace conn "farallones")
    (reify adapters.core/EventConsumer
      (adapters.core/consume [_ event]
        (do (cql/insert conn "log_entries"
          (adapters.core/flatten-uniqueLogEvent event)) "Cassandra insert completed.")))))

