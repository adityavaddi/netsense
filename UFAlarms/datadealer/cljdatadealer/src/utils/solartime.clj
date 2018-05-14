(ns utils.solartime
  (:use clojure.tools.logging)
  (:require [clj-time.core :as t]
            [clj-time.format :as fmt]
            [clojure.string :as str])
  (:import (com.luckycatlabs.sunrisesunset SunriseSunsetCalculator)
           (com.luckycatlabs.sunrisesunset.dto Location)
           (org.joda.time DateTime DateTimeZone)
           (org.joda.time.format ISODateTimeFormat)
           (com.sensity.netsense.geolocation TimezoneMapper)))

(defn sunset
  [latitude longitude date]
  (let [location (Location. latitude longitude)
        calculator (SunriseSunsetCalculator. location (.toTimeZone (.getZone date)))
        calendar (.getOfficialSunsetCalendarForDate calculator (.toGregorianCalendar date))]
    (DateTime. calendar)))

(defn sunrise
  [latitude longitude date]
  (let [location (Location. latitude longitude)
        calculator (SunriseSunsetCalculator. location (.toTimeZone (.getZone date)))
        calendar (.getOfficialSunriseCalendarForDate calculator (.toGregorianCalendar date))]
    (DateTime. calendar)))

(defn sunset-in-milliseconds
  [latitude longitude date]
  (.getMillis (sunset latitude longitude date)))

(defn sunrise-in-milliseconds
  [latitude longitude date]
  (.getMillis (sunrise latitude longitude date)))

(defn match-operator
  [func operator offset]
  (cond
    (= offset 0) (fn [latitude longitude date] (func latitude longitude date))
    :else
      (case operator
        "+" (fn [latitude longitude date] (t/plus  (func latitude longitude date) (t/minutes offset)))
        "-" (fn [latitude longitude date] (t/minus (func latitude longitude date) (t/minutes offset)))
        ""  (fn [latitude longitude date] (t/plus  (func latitude longitude date) (t/minutes offset))))))

(defn parse-time
  [expr]
  (.toLocalTime (fmt/parse (ISODateTimeFormat/hourMinuteSecond) expr)))

(defn match-event
  [event operator offset]
  (case event
    "sunrise" (match-operator sunrise operator offset)
    "sunset"  (match-operator sunset operator offset)))

(defn parse-expr
  [expr]
  (let [[junk event operator offset] (re-matches #"([a-z]*)([-+]*)([0-9]*)" expr)]
    [event operator offset]))

(defn resolve-expr
  [expr]
  (let [expr-lower (.toLowerCase (str expr))]
    (if (or (.startsWith expr-lower "sunset") (.startsWith expr-lower "sunrise"))
      (let [[event operator offset] (parse-expr expr-lower)
             offsetval (if (.isEmpty offset) 0 (read-string offset))]
        (match-event event operator offsetval))
    (fn [latitude longitude date] (.withTime date (parse-time expr-lower))))))

(defn get-time-zone
  [latitude longitude]
  {:pre [(and latitude
              longitude)]}
  (let [timezone (TimezoneMapper/latLngToTimezoneString (read-string latitude)
                                                        (read-string longitude))]
    (if (= timezone "unknown")
      (DateTimeZone/forID "UTC")
      (DateTimeZone/forID timezone))))


(defn convert-expression-to-timestamp
  [{:as action
    :strs [time level qualifiers]
    :or {qualifiers 0}}
   latitude longitude date]
  {:pre [action time level]}
  (let [time-lower (.toLowerCase (str time))]
    ;(if (.startsWith time-lower "photocell")
    ;  (if (= time-lower "photocell_high")
    ;    {:qualifiers 2 :level level}
    ;    {:qualifiers 0 :level level})
      (let [parser (resolve-expr time)
            timestamp (parser latitude longitude date)
            ;level (read-string value)
            utctime (.toDateTime timestamp (DateTimeZone/UTC))]
        (debugf "conversion expr %s level %s timestamp %s date %s"
               time
               (str level)
               (str timestamp)
               (str utctime))
        ; Device time is UTC, convert
        {:timestamp utctime
         :level level
         :qualifiers qualifiers})
    ;  )
    ))
