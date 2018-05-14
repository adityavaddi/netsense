(ns ^{:doc "Event Triggered Daylight Harvesting"}
    appl.et-daylight-harvesting.spec
  (:require [clojure.core.match :refer [match]]
            [clojure.future :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))

;; See: https://xeranet.atlassian.net/wiki/pages/viewpage.action?pageId=123945511
;;      https://xeranet.atlassian.net/wiki/pages/viewpage.action?pageId=122966762

(s/def :etdh/uuid (s/with-gen string?
                    #(gen/fmap str (s/gen uuid?))))
(s/def :etdh/etdhprofileid :etdh/uuid)

(s/def :etdh/name string?)

(def min-lux 0)
(def max-lux 64000)
(def default-min-lux 10)
(s/def :etdh/lux (s/int-in min-lux max-lux))
(s/def :etdh/high-lux :etdh/lux)
(s/def :etdh/low-lux :etdh/lux)
(s/def :etdh/min-lux :etdh/lux)

(def min-driver 0)
(def max-driver 100)
(def default-high-driver 0)
(s/def :etdh/driver-level (s/int-in min-driver (inc max-driver)))
(s/def :etdh/high-driver :etdh/driver-level)
(s/def :etdh/low-driver :etdh/driver-level)
(s/def :etdh/min-driver :etdh/driver-level)

(def min-fast-poll 30)
(def max-fast-poll 120)
(def default-fast-poll 30)
(s/def :etdh/fast-poll (s/int-in min-fast-poll (inc max-fast-poll)))
(def min-slow-poll 60)
(def max-slow-poll 3600)
(def default-slow-poll (* 10
                          60))
(s/def :etdh/slow-poll (s/int-in min-slow-poll (inc max-slow-poll)))

(defn format-hms
  [[h m s]]
  (format "%02d:%02d:%02d" h m s))
(s/def :etdh/hms (s/with-gen (s/and string?
                                    #(re-matches #"^(([0-1]\d|2[0-3]):[0-5]\d:[0-5]\d)$"
                                                 %))
                   #(gen/fmap format-hms
                              (gen/tuple (s/gen (s/int-in 0 24))
                                         (s/gen (s/int-in 0 60))
                                         (s/gen (s/int-in 0 60))))))
(def min-sun-offset 1)
(def max-sun-offset 300)
(s/def :etdh/sunoffset (s/cat :op (s/? #{"+" "-"})
                              :h (s/int-in min-sun-offset
                                           max-sun-offset)))
(s/def :etdh/suntime (s/with-gen (s/and string?
                                        #(re-matches #"^(sunrise|sunset)((\+|\-)\d+)?$"
                                                     %))
                       #(gen/fmap (fn [str-tuple]
                                    (apply str str-tuple))
                                  (s/gen (s/spec (s/cat :s #{"sunrise"
                                                             "sunset"}
                                                        :o (s/? :etdh/sunoffset)))))))
(s/def :etdh/time (s/or :hms :etdh/hms
                        :suntime :etdh/suntime))
(s/def :etdh/beginTime :etdh/time)
(s/def :etdh/endTime :etdh/time)

(s/def :etdh/time-window (s/keys :req-un [:etdh/beginTime
                                          :etdh/endTime]))

(def max-schedule-windows 6)
(def default-schedule [{:beginTime "sunrise-90"
                        :endTime "sunset+90"}])
(s/def :etdh/scheduled (s/coll-of :etdh/time-window
                                  :min-count 1
                                  :max-count max-schedule-windows
                                  :into #{}))

(defn high-low-lux-constraint
  [{:keys [high-lux
           low-lux]}]
  (>= (- high-lux
         low-lux)
      32))
(defn lux-constraint
  [{:keys [high-lux
           low-lux
           min-lux]
    :or {min-lux default-min-lux}}]
  (> high-lux low-lux min-lux))
(defn driver-constraint
  [{:keys [high-driver
           low-driver
           min-driver]
    :or {high-driver default-high-driver}}]
  (< high-driver low-driver min-driver))
(defn poll-constraint
  [{:keys [fast-poll
           slow-poll]
    :or {fast-poll default-fast-poll
         slow-poll default-slow-poll}}]
  (< (Math/max fast-poll
               (dec min-slow-poll))
     slow-poll))
(s/def :etdh/etdhprofile (s/and (s/keys :req-un [:etdh/etdhprofileid
                                                 :etdh/name
                                                 :etdh/high-lux
                                                 :etdh/low-lux
                                                 :etdh/low-driver
                                                 :etdh/min-driver]
                                        :opt-un [:etdh/scheduled
                                                 :etdh/min-lux
                                                 :etdh/fast-poll
                                                 :etdh/slow-poll
                                                 :etdh/high-driver])
                                lux-constraint
                                high-low-lux-constraint
                                driver-constraint
                                poll-constraint))

(defn parse-problem
  [{:keys [path
           pred
           val]
    :as problem}]
  (match [path pred]
         [[] ([fn [%] ([contains? '% k] :seq)] :seq)] (format "%s is a required field."
                                                              (name k))
         [[(k :guard #{:etdhprofileid
                       :name})] _] (format "%s must be a string. Received: %s."
                                           (name k)
                                           val)
         [[k :guard #{:high-lux
                      :low-lux
                      :min-lux}] _] (format "%s > high-lux > low-lux > min-lux >= %s. Received %s = %s."
                                            max-lux
                                            min-lux
                                            (name k)
                                            val)
         [[k :guard #{:high-driver
                      :low-driver
                      :min-driver}] _] (format "%s <= high-driver < low-driver < min-driver <= %s. Received %s = %s."
                                               min-driver
                                               max-driver
                                               (name k)
                                               val)
         [[:slow-poll] _] (format "slow-poll value must be between max(fast-poll, %s) and %s seconds. Received: %s."
                                  min-slow-poll
                                  max-slow-poll
                                  val)
         [[:fast-poll] _] (format "fast-poll value must be between %s and %s seconds. Received: %s."
                                  min-fast-poll
                                  max-fast-poll
                                  val)
         [([:scheduled] :seq) _] (format "scheduled must have both beginTime and endTime. Received: %s."
                                         (-> val
                                             keys
                                             first
                                             name))
         [([:scheduled k _] :seq) _] (format "%s must be well formatted (HH:MM:SS or sunset-30 or sunrise). Received: %s."
                                             (name k)
                                             val)
         [_ (_ :guard #(= "lux-constraint"
                          (name %)))] "high-lux > low-lux > min-lux."
         [_ (_ :guard #(= "high-low-lux-constraint"
                          (name %)))] "high-lux must be 32 lux greater than low-lux."
         [_ (_ :guard #(= "driver-constraint"
                          (name %)))] "high-driver < low-driver < min-driver."
         [_ (_ :guard #(= "poll-constraint"
                          (name %)))] (format "max(fast-poll, %s) < slow-poll."
                                              min-slow-poll)

         :else (throw (ex-info "Could not parse problem"
                               problem))))
