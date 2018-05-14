(ns utils.date_test

  (:require
    [clojure.tools.logging :refer :all]
    [clojure
     [test :refer :all]]
    [clj-time.core :as t]
    [clj-time.format :as f]
    [clj-time.coerce :as c]))

(defn convert
  [timestamp]
  (let [timestamp-millis (spy :debug (quot (long timestamp) 1000))
        datetime (spy :debug (c/from-long timestamp-millis))
        formatter (spy :debug (f/formatters :date))
        parsed (spy :debug (f/unparse formatter datetime))]
    parsed))

(deftest test-timestamp-to-date

  (let
    [vals [[1490828501527264 "2017-03-29"]
           [1507060635000000 "2017-10-03"]
           [1496964335136000 "2017-06-08"]
           [1507066318351542 "2017-10-03"]
           [1507066333081910 "2017-10-03"]]

     ]
    (doall (map
            (fn [[timestamp date]]
              (is (= (convert timestamp) date)))
            vals))))
