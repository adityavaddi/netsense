(ns logging.test.core
  (:use [clojure.test])
  (:use clojure.tools.logging)
  (:require [logging.activitylogger :as al]
            [utils.neo4j-fixture :as neo4j]
            [clj-time.core :as t]
            [clj-time.format :as f]
            [utils.cassandra_fixture :as cass]))

(use-fixtures :once (join-fixtures [cass/cassandra-fixture neo4j/neo4j-fixture]))

(with-test
  (defn actlogsofsites[]
    (let [datemin (f/unparse (f/formatters :date-time-no-ms) (t/minus (t/now) (t/days 1)))
          datemax (f/unparse (f/formatters :date-time-no-ms) (t/plus (t/now) (t/minutes 10)))]
      (try
        (al/getactivitylogs "uberuser" "uberorg" "ubersite" {:datemin datemin :datemax datemax})
        (catch Exception e (error (str "actlogsofsites: " (.getMessage e)))))))
  (is (not (nil? (actlogsofsites))))
  (debug (str "Activity Logs of sites: " (actlogsofsites))))
