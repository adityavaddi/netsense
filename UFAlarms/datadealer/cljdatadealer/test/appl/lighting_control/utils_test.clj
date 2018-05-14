(ns appl.lighting-control.utils-test
  (:require [appl.lighting-control.utils :refer :all]
            [clj-time
             [core :as clj-time]]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure
             [test :refer :all]]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]))

(defn parse-int
  [str-value]
  (Integer/parseInt str-value))

(def str->date
  (comp #(apply clj-time/today-at %)
        #(map parse-int %)
        #(.split % ":")))

(deftest in-schedule-test
  (testing "is a given time between begin and end?"
    (is (true? (in-schedule {:beginTime (str->date "00:00:00")
                             :endTime (str->date "23:59:59")})))
    (is (true? (in-schedule {:beginTime (str->date "00:00:01")
                             :endTime (str->date "00:00:00")})))
    (let [evening-through-morning {:beginTime (str->date "18:00:00")
                                   :endTime (str->date "07:00:00")}]
      (with-redefs [get-now #(str->date "19:00:00")]
        (is (true? (in-schedule evening-through-morning))))
      (with-redefs [get-now #(str->date "06:00:00")]
        (is (true? (in-schedule evening-through-morning))))
      (with-redefs [get-now #(str->date "17:00:00")]
        (is (false? (in-schedule evening-through-morning))))
      (with-redefs [get-now #(str->date "08:00:00")]
        (is (false? (in-schedule evening-through-morning)))))))

(defn mock-in-schedule
  [t1 t2 t3]
  (with-redefs [get-now #(do t2)]
    (in-schedule {:beginTime t1
                  :endTime t3})))

(defn seconds-to-hms
  [seconds]
  (let [hours (quot seconds 3600)
        minutes (mod (quot seconds 60) 60)
        seconds (mod seconds 60)]
    [hours minutes seconds]))

(defn seconds-to-jodatime
  [seconds]
  (apply clj-time/today-at
         (seconds-to-hms seconds)))

(deftest in-schedule-property-test
  ;; Generate three unique, sorted numbers (`t1`, `t2`, `t3`), and map
  ;; them onto `00:00:00` through `23:59:59`. This creates a `t2`
  ;; between `t1` and `t3`.

  ;; To address the wraparound cornercase (e.g., the valid expression
  ;; of the four hour window between 10pm and 2am), exploit the
  ;; toroidal nature and rotate the timestamp array left or right
  ;; once. (i.e., `t1` is between `t3` and `t2`, `t3` is between `t2`
  ;; and `t1`).

  ;; To test negative, swap the "middle" for either the first or last
  ;; timestamp, as well as swapping the first and last timestamps.
  (checking "in-schedule" 1000
            [[t1 t2 t3] (gen/fmap #(map seconds-to-jodatime %)
                                  (s/gen (s/coll-of (s/int-in 0
                                                              (* 24
                                                                 60
                                                                 60))
                                                    :kind set?
                                                    :into (sorted-set)
                                                    :count 3)))]
            (is (= true
                   (mock-in-schedule t1 t2 t3)))
            (is (= false
                   (mock-in-schedule t2 t1 t3)))
            (is (= false
                   (mock-in-schedule t1 t3 t2)))
            (is (= false
                   (mock-in-schedule t3 t2 t1)))

            (is (= true
                   (mock-in-schedule t3 t1 t2)))
            (is (= false
                   (mock-in-schedule t1 t3 t2)))
            (is (= false
                   (mock-in-schedule t3 t2 t1)))
            (is (= false
                   (mock-in-schedule t2 t1 t3)))

            (is (= true
                   (mock-in-schedule t2 t3 t1)))
            (is (= false
                   (mock-in-schedule t3 t2 t1)))
            (is (= false
                   (mock-in-schedule t2 t1 t3)))
            (is (= false
                   (mock-in-schedule t1 t3 t2)))))
