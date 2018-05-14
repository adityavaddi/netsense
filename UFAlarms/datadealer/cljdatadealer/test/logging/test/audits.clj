(ns logging.test.audits
  (:use [clojure.test])
  (:use clojure.tools.logging)
  (:require [logging.activitylogger :as al]
            [utils.neo4j-fixture :as neo4j]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [utils.cassandra_fixture :as cass]))

(use-fixtures :once (join-fixtures [cass/cassandra-fixture neo4j/neo4j-fixture]))

(with-test
  (defn audits[]
    (let [datemin (f/unparse (f/formatters :date-time-no-ms) (t/minus (t/now) (t/days 30)))
          datemax (f/unparse (f/formatters :date-time-no-ms) (t/plus (t/now) (t/minutes 10)))]
      (try
        (do
          (spy :debug (for [x (range 1 10009)]
            (al/log "user" {:activity "pingNode"  :targettype "Node" :targetid "N013341e5" :message (str "ping " x) :userid "uberuser"})))
          (al/getactivitylogs "uberuser" "uberorg" "ubersite" {:datemin datemin :datemax datemax}))
        (catch Exception e (error (str "audits: " (.getMessage e)))))))
  (is  (=(count (audits)) 0)))
