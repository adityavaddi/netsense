(ns utils.test.solartime
  (:use [clojure.test])
  (:use clojure.tools.logging)
  (:use utils.solartime)
  (:require [clj-time.core :as t]
            [clj-time.predicates :as p])
  (:import (org.joda.time DateTime DateTimeZone)))

(deftest test-parse-sunrise-sunset
  (is (= (parse-expr "sunset+30") ["sunset" "+" "30"]))
  (is (= (parse-expr "sunset-30") ["sunset" "-" "30"]))
  (is (= (parse-expr "sunset30") ["sunset" "" "30"]))

  (is (= (parse-expr "sunrise+30") ["sunrise" "+" "30"]))
  (is (= (parse-expr "sunrise-30") ["sunrise" "-" "30"]))
  (is (= (parse-expr "sunrise30") ["sunrise" "" "30"]))

  (is (= (parse-expr "sunset") ["sunset" "" ""]))
  (is (= (parse-expr "sunrise") ["sunrise" "" ""]))

  (is (= (parse-expr "junk") ["junk" "" ""])))


(deftest test-parse-time
  (is (= (parse-time "19:10:05") (t/local-time 19 10 5))))


(deftest test-local-sunset

  (let [
        latitude "37.380996" ; Sunnyvale, CA
        longitude "-121.992299"
        tz (get-time-zone latitude longitude)
        timestamp (DateTime. 1450832040000 tz) ; Tue Dec 22 16:54:00 PST 2015
        halfhour-ms (t/minutes 30)
        date (DateTime. 2015 12 22 0 0 0 tz)
        timestamputc (.toDateTime timestamp (DateTimeZone/UTC))

        local-sunset-ms (sunset-in-milliseconds latitude longitude date)
        sunset-plus-ms ((resolve-expr "sunset+30") latitude longitude date)
        sunset-nosign-ms ((resolve-expr "sunset30") latitude longitude date)
        sunset-ms ((resolve-expr "sunset") latitude longitude date)
        sunset-minus-ms ((resolve-expr "sunset-30") latitude longitude date)
        local-time ((resolve-expr "16:54:00") latitude longitude date)

        sunset-minus-expr-map (convert-expression-to-timestamp {"time" "sunset-30" "level" 50} latitude longitude date)]

    (spyf :info "sunset %s" local-sunset-ms)
    (spyf :info "sunset plus %s" sunset-plus-ms)

    (is (= local-sunset-ms (.getMillis timestamp)))
    (is (p/same-date? sunset-plus-ms (t/plus timestamp halfhour-ms)))
    (is (p/same-date? sunset-nosign-ms (t/plus timestamp halfhour-ms)))
    (is (p/same-date? sunset-ms timestamp))
    (is (p/same-date? sunset-minus-ms (t/minus timestamp halfhour-ms)))
    (is (p/same-date? local-time timestamp))
    (is (p/same-date? (:timestamp sunset-minus-expr-map) (t/minus timestamputc halfhour-ms)))
    (is (= (:level sunset-minus-expr-map) 50))
    ))


(deftest test-local-sunrise

  (let [latitude "37.380996" ; Sunnyvale, CA
        longitude "-121.992299"
        tz (get-time-zone latitude longitude)
        timestamp (DateTime. 1450797540000 tz) ; Tue Dec 22 07:19:00 PST 2015
        halfhour-ms (t/minutes 30)
        date (DateTime. 2015 12 22 0 0 0 tz)
        timestamputc (.toDateTime timestamp (DateTimeZone/UTC))

        local-sunrise-ms (sunrise-in-milliseconds latitude longitude date)
        sunrise-plus-ms ((resolve-expr "sunrise+30") latitude longitude date)
        sunrise-nosign-ms ((resolve-expr "sunrise30") latitude longitude date)
        sunrise-ms ((resolve-expr "sunrise") latitude longitude date)
        sunrise-minus-ms ((resolve-expr "sunrise-30") latitude longitude date)
        local-time ((resolve-expr "07:19:00") latitude longitude date)

        sunrise-plus-expr-map (convert-expression-to-timestamp {"time" "sunrise+30" "level" 50} latitude longitude date)]

    (spyf :info "sunrise %s" local-sunrise-ms)
    (spyf :info "sunrise plus %s" sunrise-plus-ms)

    (is (= local-sunrise-ms (.getMillis timestamp)))
    (is (p/same-date? sunrise-plus-ms (t/plus timestamp halfhour-ms)))
    (is (p/same-date? sunrise-nosign-ms (t/plus timestamp halfhour-ms)))
    (is (p/same-date? sunrise-ms timestamp))
    (is (p/same-date? sunrise-minus-ms (t/minus timestamp halfhour-ms)))
    (is (p/same-date? local-time timestamp))
    (is (p/same-date? (:timestamp sunrise-plus-expr-map) (t/plus timestamputc halfhour-ms)))
    (is (= (:level sunrise-plus-expr-map) 50))
    ))