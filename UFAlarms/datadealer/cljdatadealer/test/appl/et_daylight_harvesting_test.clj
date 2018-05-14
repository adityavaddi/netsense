(ns appl.et-daylight-harvesting-test
  (:require [appl.et-daylight-harvesting :as sut]
            [appl.et-daylight-harvesting.crud-test :as crud-test]
            [appl.et-daylight-harvesting.spec :as etdh-spec]
            [appl.lighting-control
             [utils :as light-utils]
             [utils-test :as light-utils-test]]
            [appl.daylight_harvest :as dh]
            [appl.proximity-dimming :as pd]
            [clj-time.coerce :as coerce-time]
            [clojure
             [test :refer :all]
             [walk :refer [keywordize-keys]]]
            [clojure.core.async :as async]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]
            [com.stuartsierra.component :as component]
            [dealer.devsvcctrl-test :as dctrl-test]
            [mqtt.fixture :as mqtt]
            [kafka.mock :as kafka]
            [neowrap.neowrapper :as neo4j]
            [utils
             [cape :as cape]
             [cape-test :refer [vectors-to-sets
                                run
                                *user*
                                uuid
                                create-test-org
                                create-test-site
                                default-test-node
                                create-test-node
                                create-test-lighting-group
                                default-test-etdhprofile
                                create-test-etdhprofile]]
             [cassandra_fixture :as cass]
             [neo4j-fixture :as neo4j-fixture]]))

;;; See: https://xeranet.atlassian.net/wiki/pages/viewpage.action?pageId=122966762

(use-fixtures :once (join-fixtures [cass/cassandra-fixture
                                    neo4j-fixture/neo4j-fixture
                                    kafka/kafka-fixture
                                    mqtt/mqtt-fixture
                                    dctrl-test/device-service-control-fixture
                                    dctrl-test/lfs-timeout-fixture
                                    utils.cape-test/scoped-thread-pool-fixture
                                    utils.cape-test/mock-google-api]))

(def test-etdhprofile
  {:etdhprofileid "07035604-0af1-450c-ab5e-c7c08102a403"
   :name "Example DHProfile"
   :high-lux 100
   :high-driver 0
   :low-lux 50
   :low-driver 20
   :min-lux 10
   :min-driver 80
   :fast-poll 30
   :slow-poll 600
   :scheduled [{:beginTime "sunrise-2"
                :endTime "sunset+2"}
               {:beginTime "20:00:00"
                :endTime "23:00:00"}]})

;; #1
(deftest confirm-example-profile-test
  (is (= nil
         (s/explain-data :etdh/etdhprofile
                         test-etdhprofile)))
  (is (= nil
         (sut/explain-etdhprofile test-etdhprofile)))
  (is (= nil
         (-> test-etdhprofile
             sut/with-etdhprofile-defaults
             sut/explain-etdhprofile))))

;; #9
(deftest support-multiple-timewindows-test
  ;; Generate a profile such that "now" is not in any of the windows
  ;; Generate a profile such that "now" *is* in one of the windows

  ;; To test a set of time windows, first pick a time range, with two
  ;; sub-ranges, with holes in each. These holes are "active" times
  ;; inside the subranges and "inactive" outside.

  ;; To test negative, select a time outside parent range as the
  ;; current time. For a less naieve selection of the current time,
  ;; establish what the effective computed sub-ranges are
  ;; (de-duplicate and extend when time ranges overlap) and pick a
  ;; time in the remaining space that's still inside the over-arching
  ;; range. And finally, map onto `00:00:00` through `23:59:59`. Using
  ;; rotations for validaton of the wraparound cornercase is still
  ;; preserved.
  (checking "etdhprofile in-schedule" 100
            [[inactive1
              range1-begin
              range1-active
              range1-end
              inactive2
              range2-begin
              range2-active
              range2-end
              inactive3] (gen/fmap #(map (comp etdh-spec/format-hms
                                               light-utils-test/seconds-to-hms)
                                         %)
                                   (s/gen (s/coll-of (s/int-in 0
                                                               (* 24
                                                                  60
                                                                  60))
                                                     :kind set?
                                                     :into (sorted-set)
                                                     :count 9)))]
            (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;")
            (let [orgid (create-test-org)
                  siteid (create-test-site orgid
                                           {;; Edinburgh of the Seven
                                            ;; Seas, one of the few UK
                                            ;; countries where the
                                            ;; timezone is UTC, not
                                            ;; GMT. This makes
                                            ;; debugging easier as
                                            ;; there's no adjustment
                                            ;; for timezones when
                                            ;; everything starts in
                                            ;; UTC to begin with.
                                            :latitude "-37.067278"
                                            :longitude "-12.31"})]
              (let [scheduled [{:beginTime range1-begin
                                :endTime range1-end}
                               {:beginTime range2-begin
                                :endTime range2-end}]
                    etdhprofile-1 (create-test-etdhprofile orgid
                                                           siteid
                                                           {:scheduled scheduled})]
                (is (= nil
                       (s/explain-data :etdh/scheduled
                                       scheduled)))
                (doseq [inactive [inactive1
                                  inactive2
                                  inactive3]]
                  (with-redefs [light-utils/get-now #(light-utils-test/str->date inactive)]
                    (is (= false
                           (sut/in-schedule? etdhprofile-1)))))
                (doseq [active [range1-active
                                range2-active]]
                  (with-redefs [light-utils/get-now #(light-utils-test/str->date active)]
                    (is (= true
                           (sut/in-schedule? etdhprofile-1))))))
              ;; Testing the corner case when we have a rotation. Note
              ;; the `true`/`false` pairs are swapped when compared to
              ;; above.
              (let [scheduled [{:beginTime range2-end
                                :endTime range1-begin}
                               {:beginTime range1-end
                                :endTime range2-begin}]
                    etdhprofile-2 (create-test-etdhprofile orgid
                                                           siteid
                                                           {:scheduled scheduled})]
                (is (= nil
                       (s/explain-data :etdh/scheduled
                                       scheduled)))
                (doseq [inactive [inactive1
                                  inactive2
                                  inactive3]]
                  (with-redefs [light-utils/get-now #(light-utils-test/str->date inactive)]
                    (is (= true
                           (sut/in-schedule? etdhprofile-2)))))
                (doseq [active [range1-active
                                range2-active]]
                  (with-redefs [light-utils/get-now #(light-utils-test/str->date active)]
                    (is (= false
                           (sut/in-schedule? etdhprofile-2)))))))))

;; #2
(deftest trgl-sensor-mode-test
  ;; See: https://xeranet.atlassian.net/wiki/spaces/KB/pages/16908349/Micronode+Sensor+and+Unit+Names
  (let [orgid (create-test-org)
        siteid (create-test-site orgid)
        nodes (->> ["unode-v2"
                    "unode-v3"
                    "unode-v4"
                    "unode-v5"
                    "unode-v6"]
                   (map (juxt identity
                              #(create-test-node orgid
                                                 siteid
                                                 {:model %})))
                   (into {}))]
    ;; Verify v2/6 has no `trgl` sensor.
    (doseq [[model nodeid] (select-keys nodes
                                        ["unode-v2"
                                         "unode-v6"])
            :let [ret (keywordize-keys (cape/send-config-to-node {:nodeid nodeid
                                                                  :clientType model}))]]
      (is (= (update ret
                     :kvpairs
                     dissoc :sensor.trgl.mode)
             ret)))
    ;; Verify v3-5 have `trgl` mode = 3
    (doseq [[model nodeid] (select-keys nodes
                                        ["unode-v3"
                                         "unode-v4"
                                         "unode-v5"])
            :let [ret (keywordize-keys (cape/send-config-to-node {:nodeid nodeid
                                                                  :clientType model}))]]
      (is (= (assoc-in ret
                       [:kvpairs :sensor.trgl.mode]
                       3)
             ret)))))

;; #5
(deftest slope-intercept-test
  (is (= [1 0]
         (sut/slope-intercept 0 0
                              1 1)))
  (is (= [1 -1]
         (sut/slope-intercept 1 0
                              2 1)))
  (is (= [1 1]
         (sut/slope-intercept 0 1
                              1 2)))
  (is (= [2 0]
         (sut/slope-intercept 0 0
                              1 2)))
  (is (= [1/2 0]
         (sut/slope-intercept 0 0
                              2 1))))

(deftest slope-intercept-property-test
  ;; Select some slope and intercept, two sample X coordinates, and
  ;; verify `slope-intercept` returns slope/intercept when fed
  ;; coordinates.
  (checking "slope-intercept" 1000
            [slope (gen/ratio)
             intercept (gen/int)
             x1 (gen/int)
             x2 (gen/such-that #(not= x1 %)
                               (gen/int))]
            (is (= [slope intercept]
                   (sut/slope-intercept x1 (-> x1
                                               (* slope)
                                               (+ intercept))
                                        x2 (-> x2
                                               (* slope)
                                               (+ intercept)))))))

(deftest calculate-driver-level-with-test-etdhprofile-test
  (is (= 80
         (sut/calculate-driver-level test-etdhprofile -10)
         (sut/calculate-driver-level test-etdhprofile 0)
         (sut/calculate-driver-level test-etdhprofile 10)))
  (is (= 65
         (sut/calculate-driver-level test-etdhprofile 20)))
  (is (= 50
         (sut/calculate-driver-level test-etdhprofile 30)))
  (is (= 35
         (sut/calculate-driver-level test-etdhprofile 40)))
  (is (= 20
         (sut/calculate-driver-level test-etdhprofile 50)))
  (is (= 16
         (sut/calculate-driver-level test-etdhprofile 60)))
  (is (= 12
         (sut/calculate-driver-level test-etdhprofile 70)))
  (is (= 8
         (sut/calculate-driver-level test-etdhprofile 80)))
  (is (= 4
         (sut/calculate-driver-level test-etdhprofile 90)))
  (is (= 0
         (sut/calculate-driver-level test-etdhprofile 100)
         (sut/calculate-driver-level test-etdhprofile 110)
         (sut/calculate-driver-level test-etdhprofile 120))))

(deftest calculate-driver-level-property-test
  ;; For some etdhprofile, `calculate-driver-level` returns expected
  ;; driver level for explicitly stated profile values. For any two
  ;; lux levels, the driver levels are equal or decreasing. For values
  ;; outside the profile lux values, driver levels are constant.
  (checking "calculate-driver-level" 1000
            [{:keys [high-lux high-driver
                     low-lux  low-driver
                     min-lux  min-driver]
              :as etdhprofile} (gen/fmap sut/with-etdhprofile-defaults
                                         (s/gen :etdh/etdhprofile))

             lux-1 (gen/int)]
            (is (= high-driver
                   (sut/calculate-driver-level etdhprofile
                                               high-lux)))
            (is (= low-driver
                   (sut/calculate-driver-level etdhprofile
                                               low-lux)))
            (is (= min-driver
                   (sut/calculate-driver-level etdhprofile
                                               min-lux)))

            (is (>= (sut/calculate-driver-level etdhprofile
                                                (dec lux-1))
                    (sut/calculate-driver-level etdhprofile
                                                lux-1)
                    (sut/calculate-driver-level etdhprofile
                                                (inc lux-1))))

            (is (= high-driver
                   (sut/calculate-driver-level etdhprofile
                                               (inc high-lux))
                   (sut/calculate-driver-level etdhprofile
                                               (inc (inc high-lux)))))
            (is (= min-driver
                   (sut/calculate-driver-level etdhprofile
                                               (dec min-lux))
                   (sut/calculate-driver-level etdhprofile
                                               (dec (dec min-lux)))))))

(deftest component-calls-startup-shutdown-test
  (let [sys (atom (component/system-map
                   :etdh-scheduler (sut/new-etdh-scheduler)))
        invocation-log (atom [])]
    (with-redefs [sut/startup! #(swap! invocation-log
                                       conj :startup)
                  sut/shutdown! #(swap! invocation-log
                                        conj :shutdown)]
      (is (= []
             @invocation-log))
      (swap! sys component/start)
      (is (= [:startup]
             @invocation-log))
      (swap! sys component/stop)
      (is (= [:startup :shutdown]
             @invocation-log)))))

(deftest startup-adds-profiles-test
  (let [invocation-log (atom [])]
    (with-redefs [sut/consume-schedule-channel! (constantly nil)
                  sut/get-all-etdhprofileids-with-site #(do [:etdhprofileid])
                  sut/add-profile! #(swap! invocation-log
                                           conj %&)]
      (sut/startup!))
    (is (= [[:etdhprofileid]]
           @invocation-log))))

(deftest profile-creation-deletion-callbacks-test
  (let [orgid (create-test-org)
        siteid (create-test-site orgid)
        invocation-log (atom [])]
    (with-redefs [sut/add-profile! #(swap! invocation-log
                                           conj {:add %&})
                  sut/remove-profile! #(swap! invocation-log
                                              conj {:remove %&})]
      (testing "ETDHProfile creation makes callback"
        (let [etdhprofileid (create-test-etdhprofile orgid siteid)]
          (is (= [{:add [etdhprofileid]}]
                 @invocation-log))

          (testing "ETDHProfile deletion makes callback"
            (let [{:keys [success]} (run {:type "deleteETDHProfile"
                                          :user *user*
                                          :orgprops {:orgid orgid}
                                          :siteprops {:siteid siteid}
                                          :etdhprofileprops {:etdhprofileid etdhprofileid}})]
              (is (= true
                     success))
              (is (= [{:add [etdhprofileid]}
                      {:remove [etdhprofileid]}]
                     @invocation-log)))))))))

(deftest profile-activation-test
  (testing "`add-profile!` calls helper functions"
    (let [invocation-log (atom [])]
      (with-redefs [sut/schedule-next-boundary! (constantly nil)
                    sut/activate-profile! #(swap! invocation-log conj [:activate %&])
                    sut/deactivate-profile! #(swap! invocation-log conj [:deactivate %&])]
        (with-redefs [sut/in-schedule? (constantly false)]
          (sut/add-profile! :etdhprofileid))
        (is (= [[:deactivate [:etdhprofileid]]]
               @invocation-log))

        (with-redefs [sut/in-schedule? (constantly true)]
          (sut/add-profile! :etdhprofileid))
        (is (= [[:deactivate [:etdhprofileid]]
                [:activate [:etdhprofileid]]]
               @invocation-log)))))
  (testing "`:active?` flag is appropriately set"
    (with-redefs [sut/profile-state (atom {:p {}})
                  sut/get-etdhprofile (constantly {:site {:latitude "-37.067278"
                                                          :longitude "-12.31"}})
                  light-utils/reset-lights (constantly nil)
                  sut/schedule-fsm-times (constantly nil)
                  sut/run-controller! (constantly nil)]
      (sut/activate-profile! :p)
      (is (= (-> @sut/profile-state
                 (get :p)
                 (assoc :active? true))))

      (sut/deactivate-profile! :p)
      (is (= (-> @sut/profile-state
                 (get :p)
                 (assoc :active? false))))))
  (testing "FSM times are added on activation, removed on deactivation"
    (let [invocation-log (atom [])
          schedule-callbacks {:sunset-90  [:sunset-90  :p 0 java.util.concurrent.TimeUnit/MILLISECONDS]
                              :sunrise-90 [:sunrise-90 :p 0 java.util.concurrent.TimeUnit/MILLISECONDS]
                              :midnight   [:midnight   :p 0 java.util.concurrent.TimeUnit/MILLISECONDS]}]
      (with-redefs [sut/profile-state (atom {:p {}})
                    sut/get-etdhprofile (constantly {:site {:latitude "-37.067278"
                                                            :longitude "-12.31"}})
                    light-utils/resolve-symbolic-time (constantly nil)
                    sut/millis-until-next-time-of-day (constantly 0)
                    sut/schedule-shim #(do
                                         (swap! invocation-log conj %&)
                                         [:schedule-callback %&])
                    sut/run-controller! (constantly nil)
                    light-utils/reset-lights (constantly nil)
                    sut/schedule-cancel-shim #(swap! invocation-log conj [:cancel %&])]
        (sut/activate-profile! :p)
        (is (= (->> schedule-callbacks
                    (map (fn [[k v]]
                           [k [:schedule-callback v]]))
                    (into {}))
               (-> @sut/profile-state
                   (get :p)
                   (select-keys [:sunset-90
                                 :sunrise-90
                                 :midnight]))))
        (is (= (vals schedule-callbacks)
               @invocation-log))

        (reset! invocation-log [])
        (sut/deactivate-profile! :p)
        (is (= (->> schedule-callbacks
                    (map (fn [[k v]]
                           [:cancel [[:schedule-callback v]]])))
               @invocation-log))))))

(deftest mockable-scheduler-test
  (testing "Unmocked schedule"
    (with-redefs [sut/schedule-channel (async/chan)]
      (with-open [_ (reify
                      java.io.Closeable
                      (close [_]
                        (async/close! sut/schedule-channel)))]
        (let [p (promise)]
          (with-redefs [sut/schedule-callback-handler (fn [& _]
                                                        (deliver p :ok))]
            (let [pipeline-chan (sut/consume-schedule-channel!)]
              (with-open [_ (reify
                              java.io.Closeable
                              (close [_]
                                (async/close! pipeline-chan)))]
                (sut/schedule! :payload :p
                               1
                               java.util.concurrent.TimeUnit/MICROSECONDS)
                (is (= :ok
                       (deref p
                              500 :not-ok))))))))))
  (testing "Mock scheduler"
    (let [invocation-log (atom [])
          test-args [:callback :offset :timeunit]]
      (with-redefs [sut/schedule! #(swap! invocation-log conj %&)]
        (apply sut/schedule! test-args))
      (is (= [test-args]
             @invocation-log)))))

(deftest local-profile-state-test
  (with-redefs [sut/profile-state (atom {})
                sut/in-schedule? (constantly nil)
                sut/check-schedule! (constantly nil)]
    (testing "ETDHProfile creation adds entry to ETDH local profile state"
      (sut/add-profile! :etdhprofileid)
      (is (= (assoc @sut/profile-state
                    :etdhprofileid {})
             @sut/profile-state))

      (sut/remove-profile! :etdhprofileid)
      (is (= (dissoc @sut/profile-state
                     :etdhprofileid)
             @sut/profile-state)))))

(deftest next-schedule-test
  (testing "Correct window boundary is chosen"
    (with-redefs [sut/get-etdhprofile (constantly {:etdhprofile {:scheduled [{:beginTime "00:00:15"
                                                                              :endTime "00:00:20"}
                                                                             {:beginTime "00:00:30"
                                                                              :endTime "00:00:45"}]}
                                                   :site {:latitude "-37.067278"
                                                          :longitude "-12.31"}})]
      ;; At ":25", after the first window, we should get a request for
      ;; the next `:beginTime`, in the second window at ":30".
      (with-redefs [light-utils/get-now #(-> 25
                                             (* 1e3)
                                             long
                                             coerce-time/from-long)]
        (is (= [5000 "00:00:30"]
               (sut/next-schedule :p))))

      ;; At ":35", inside the first window, we should get a request for
      ;; the next `:endTime`, in the same window at ":45".
      (with-redefs [light-utils/get-now #(-> 35
                                             (* 1e3)
                                             long
                                             coerce-time/from-long)]
        (is (= [10000 "00:00:45"]
               (sut/next-schedule :p))))

      ;; At ":55", after the second window, we should get a request for
      ;; `:beginTime`, wrapping around to the first window at ":15".
      (with-redefs [light-utils/get-now #(-> 55
                                             (* 1e3)
                                             long
                                             coerce-time/from-long)]
        (is (= [(* 1000
                   (- (* 3600 24)
                      (- 55
                         15))) "00:00:15"]
               (sut/next-schedule :p)))))))

(deftest check-schedule-test
  (testing "Schedule next entry as expected"
    (let [invocation-log (atom [])
          schedule-args [:check-schedule :p
                         500
                         java.util.concurrent.TimeUnit/MILLISECONDS]]
      (with-redefs [sut/profile-state (atom {:p {:check-schedule :prior-schedule-check-cb}})
                    sut/in-schedule? (constantly nil)
                    sut/deactivate-profile! (constantly nil)
                    sut/next-schedule (constantly [500 :test])
                    sut/schedule-shim #(do
                                         (swap! invocation-log conj %&)
                                         [:schedule-callback %&])]
        (sut/check-schedule! :p)
        (is (= [:schedule-callback schedule-args]
               (-> @sut/profile-state
                   (get :p)
                   :check-schedule)))
        (is (= [schedule-args]
               @invocation-log)))))
  (testing "Callback calls `check-schedule`, canceling any existing callback."
    (let [invocation-log (atom [])
          sch-check-in (promise)
          sch-check-out (promise)
          sch sut/schedule-callback-handler]
      (with-redefs [sut/schedule-channel (async/chan)
                    sut/profile-state (atom {:p {}})
                    sut/next-schedule (constantly [0 :test])
                    sut/schedule-callback-handler (fn [& args]
                                                    (deref sch-check-in)
                                                    (with-open [_ (reify
                                                                    java.io.Closeable
                                                                    (close [_]
                                                                      (deliver sch-check-out :check)))]
                                                      (apply sch args)))
                    sut/schedule-cancel-shim #(swap! invocation-log conj [:cancel %&])
                    sut/check-schedule! #(swap! invocation-log conj [:check %&])]
        (let [pipeline-chan (sut/consume-schedule-channel!)]
          (with-open [_ (reify
                          java.io.Closeable
                          (close [_]
                            (async/close! sut/schedule-channel)))
                      _ (reify
                          java.io.Closeable
                          (close [_]
                            (async/close! pipeline-chan)))]
            (sut/schedule-next-boundary! :p)
            (swap! sut/profile-state update
                   :p assoc
                   :check-schedule :t2)
            (deliver sch-check-in :go)
            (is (= :check
                   (deref sch-check-out
                          5000
                          :not-checked)))
            (is (= [[:cancel [:t2]]
                    [:check [:p]]]
                   @invocation-log))))))))

(deftest profile-has-fsm-test
  (with-redefs [sut/profile-state (atom {:p {}})
                sut/get-etdhprofile (constantly {:site {:latitude "-37.067278"
                                                        :longitude "-12.31"}})
                light-utils/reset-lights (constantly nil)
                sut/schedule-fsm-times (constantly nil)
                sut/run-controller! (constantly nil)]
    (sut/activate-profile! :p)
    (is (= true
           (contains? (:p @sut/profile-state)
                      :fsm)))

    (sut/deactivate-profile! :p)
    (is (= false
           (contains? (:p @sut/profile-state)
                      :fsm)))))

(defn delay-for
  [seconds]
  (reify
    java.util.concurrent.ScheduledFuture
    (getDelay [_ _]
      seconds)))

(defn set-fsm
  [etdhprofileid end-state]
  {:pre [(#{:no-polling :hour-polling :slow-polling :fast-polling} end-state)]
   :post [(= end-state %)]}
  (with-redefs [sut/schedule-next-poll! (constantly nil)]
    (sut/handle-fsm-event! etdhprofileid :low-trigger)
    (sut/handle-fsm-event! etdhprofileid (end-state {:no-polling :high-trigger
                                                     :hour-polling :dim-light-and-late
                                                     :slow-polling :dim-light
                                                     :fast-polling :low-trigger}))))

(deftest profile-fsm-event-test
  (testing "FSM transitions states as expected"
    (with-redefs [sut/profile-state (atom {:etdhprofileid {}})
                  sut/schedule-fsm-times (constantly nil)
                  sut/get-etdhprofile (constantly {:site {:latitude "-37.067278"
                                                          :longitude "-12.31"}})
                  sut/run-controller! (constantly nil)
                  sut/schedule-next-poll! (constantly nil)]
      (sut/activate-profile! :etdhprofileid)
      (is (= nil
             (sut/handle-fsm-event! :etdhprofileid :sunrise-90)))
      (is (= :slow-polling
             (sut/handle-fsm-event! :etdhprofileid :dim-light)))
      (is (= :hour-polling
             (sut/handle-fsm-event! :etdhprofileid :dim-light-and-late)))
      (is (= :no-polling
             (sut/handle-fsm-event! :etdhprofileid :high-trigger)))
      (is (= nil
             (-> @sut/profile-state
                 :etdhprofileid
                 :mean-lux)))
      (swap! sut/profile-state
             update :etdhprofileid
             assoc :mean-lux
             [:x])
      (is (= :fast-polling
             (sut/handle-fsm-event! :etdhprofileid :low-trigger)))
      (is (= []
             (-> @sut/profile-state
                 :etdhprofileid
                 :mean-lux)))))
  (testing "FSM transitions states as expected"
    (let [fast-poll 7
          effective-run-controller-interval (- fast-poll
                                               sut/poll-response-timeout)]
      (with-redefs [sut/profile-state (atom {:etdhprofileid {}})
                    sut/get-etdhprofile (constantly {:etdhprofile {:fast-poll fast-poll}})]
        ;; Obtain an active ETDH with FSM.
        (with-redefs [sut/schedule-fsm-times (constantly nil)
                      sut/get-etdhprofile (constantly {:site {:latitude "-37.067278"
                                                              :longitude "-12.31"}})
                      sut/run-controller! (constantly nil)
                      sut/schedule-next-poll! (constantly nil)]
          (sut/activate-profile! :etdhprofileid))


        (testing "When current timer is nil, replace"
          (set-fsm :etdhprofileid :no-polling)
          (let [p (promise)]
            (with-redefs [sut/profile-state (atom (assoc-in @sut/profile-state
                                                            [:etdhprofileid :run-controller]
                                                            nil))
                          sut/schedule! #(deliver p %&)]
              (sut/handle-fsm-event! :etdhprofileid :low-trigger))
            (is (= true
                   (realized? p)))
            (is (= [:run-controller :etdhprofileid effective-run-controller-interval java.util.concurrent.TimeUnit/SECONDS]
                   (and (realized? p)
                        @p)))))

        (testing "When current timer is negative, replace"
          (set-fsm :etdhprofileid :hour-polling)
          (let [p (promise)]
            (with-redefs [sut/profile-state (atom (assoc-in @sut/profile-state
                                                            [:etdhprofileid :run-controller]
                                                            (delay-for -1)))
                          sut/schedule! #(deliver p %&)]
              (sut/handle-fsm-event! :etdhprofileid :low-trigger))
            (is (= true
                   (realized? p)))
            (is (= [:run-controller :etdhprofileid effective-run-controller-interval java.util.concurrent.TimeUnit/SECONDS]
                   (and (realized? p)
                        @p)))))

        (testing "When current timer is greater than replacement, replace"
          (set-fsm :etdhprofileid :no-polling)
          (let [p (promise)]
            (with-redefs [sut/profile-state (atom (assoc-in @sut/profile-state
                                                            [:etdhprofileid :run-controller]
                                                            (delay-for (inc effective-run-controller-interval))))
                          sut/schedule! #(deliver p %&)]
              (sut/handle-fsm-event! :etdhprofileid :low-trigger))
            (is (= true
                   (realized? p)))
            (is (= [:run-controller :etdhprofileid effective-run-controller-interval java.util.concurrent.TimeUnit/SECONDS]
                   (and (realized? p)
                        @p)))))

        (testing "When current timer is less than replacement, don't replace"
          (set-fsm :etdhprofileid :no-polling)
          (let [p (promise)]
            (with-redefs [sut/profile-state (atom (assoc-in @sut/profile-state
                                                            [:etdhprofileid :run-controller]
                                                            (delay-for (dec effective-run-controller-interval))))
                          sut/schedule! #(deliver p %&)]
              (sut/handle-fsm-event! :etdhprofileid :low-trigger))
            (is (= false
                   (realized? p)))))))))

(deftest profile-removal-resets-nodes
  (let [invocation-log (atom [])
        p {:etdhprofile {:nodes [{:nodeid :node-a}
                                 {:nodeid :node-b}]}}]
    (with-redefs [sut/profile-state (atom {:p {}})
                  light-utils/reset-lights #(swap! invocation-log conj [:reset-lights %&])
                  sut/get-etdhprofile (constantly p)]
      (sut/remove-profile! :p))
    (is (= [[:reset-lights [[:node-a :node-b]]]]
           @invocation-log))))

(deftest run-controller-test
  (testing "`activate-profile!` calls `run-controller!`"
    (let [invocation-log (atom [])]
      (with-redefs [sut/profile-state (atom {:p {}})
                    sut/schedule-fsm-times (constantly nil)
                    sut/run-controller! #(swap! invocation-log conj [:run-controller %&])
                    sut/get-etdhprofile (constantly {:site {:latitude "-37.067278"
                                                            :longitude "-12.31"}})]
        (sut/activate-profile! :p))
      (is (= [[:run-controller [:p]]]
             @invocation-log))))
  (testing "Schedule next entry as expected"
    (let [invocation-log (atom [])
          fast-poll 12
          p {:etdhprofile {:fast-poll fast-poll}}
          schedule-args [:run-controller :p
                         (- fast-poll
                            sut/poll-response-timeout)
                         java.util.concurrent.TimeUnit/SECONDS]]
      (with-redefs [sut/profile-state (atom {:p {:active? true
                                                 :fsm {:state :fast-polling}}})
                    sut/get-etdhprofile (constantly p)
                    sut/in-schedule? (constantly nil)
                    sut/deactivate-profile! (constantly nil)
                    sut/next-schedule (constantly [500 :test])
                    sut/schedule-shim #(do
                                         (swap! invocation-log conj [:schedule %&])
                                         [:schedule-callback %&])]
        (sut/run-controller! :p)
        (is (= [[:schedule schedule-args]]
               @invocation-log))
        (is (= [:schedule-callback schedule-args]
               (-> @sut/profile-state
                   (get :p)
                   :run-controller))))))
  (testing "`deactivate-profile!` cancels `:run-controller`"
    (let [invocation-log (atom [])]
      (with-redefs [sut/profile-state (atom {:p {:run-controller :p}})
                    light-utils/reset-lights (constantly nil)
                    sut/schedule-cancel-shim #(swap! invocation-log conj [:cancel %&])]
        (sut/deactivate-profile! :p))
      (is (= [[:cancel [:p]]]
             @invocation-log)))))

(deftest get-ambient-light-test
  (testing "`get-etdh-nodes`"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          [node-a node-b :as nodeList] (repeatedly 2 #(create-test-node orgid siteid))
          lg (create-test-lighting-group orgid
                                         siteid
                                         {:nodeList nodeList})
          etdhprofileid (create-test-etdhprofile orgid siteid)]
      (crud-test/apply-etdh-groups orgid
                                   siteid
                                   etdhprofileid
                                   [lg])
      (try
        (is (= {:controllers (set nodeList)
                :triggers (set nodeList)}
               (-> (sut/get-etdh-nodes etdhprofileid)
                   (update :controllers set)
                   (update :triggers set))))
        (finally
          (sut/remove-profile! etdhprofileid)))))

  (testing "`request-ambient-light` calls correct nodes"
    (let [invocation-log (atom [])
          trigger-nodes [:node-a :node-b]]
      (with-redefs [sut/profile-state (atom {:p {:active? true}})
                    sut/schedule-next-poll! (constantly nil)
                    sut/get-etdh-nodes (constantly {:triggers trigger-nodes})
                    sut/request-ambient-light #(swap! invocation-log conj %&)]
        (sut/run-controller! :p)
        (is (= [[trigger-nodes]]
               @invocation-log)))))
  (testing "`get-profileid-from-node`"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          nodeid (create-test-node orgid siteid)
          lg (create-test-lighting-group orgid
                                         siteid
                                         {:nodeList [nodeid]})
          etdhprofileid (create-test-etdhprofile orgid siteid)]
      (with-open [_ (reify
                      java.io.Closeable
                      (close [_]
                        (sut/remove-profile! etdhprofileid)))]
        (crud-test/apply-etdh-groups orgid
                                     siteid
                                     etdhprofileid
                                     [lg])
        (sut/get-profileid-from-trigger-node nodeid))))

  (testing "`handle-l-value`"
    (let [t1 (* 1000 ;; microseconds
                1000 ;; milliseconds
                15  ;; seconds
                )
          t0 (* 1000
                1000
                13)
          t2 (* 1000
                1000
                17)]
      (with-redefs [sut/profile-state (atom {:p {:active? true}})
                    sut/get-profileid-from-trigger-node (constantly :p)]
        (sut/handle-l-value! {:nodeid :my-node
                              :value 73
                              :time t1})
        (is (= {:my-node {:value 73
                          :time (pd/node-time-to-joda t1)}}
               (-> @sut/profile-state
                   (get :p)
                   :l)))

        (testing "Late arrivals should be ignored"
          (sut/handle-l-value! {:nodeid :my-node
                                :value 73
                                :time t0})
          (is (= {:my-node {:value 73
                            :time (pd/node-time-to-joda t1)}}
                 (-> @sut/profile-state
                     (get :p)
                     :l))))

        (testing "Recent values should be accepted"
          (sut/handle-l-value! {:nodeid :my-node
                                :value 73
                                :time t2})
          (is (= {:my-node {:value 73
                            :time (pd/node-time-to-joda t2)}}
                 (-> @sut/profile-state
                     (get :p)
                     :l))))

        (testing "When we have an l and are sent an l-i, we don't accept"
          (swap! sut/profile-state
                 assoc-in [:p :l :my-node :sensor]
                 "l")
          (-> @sut/profile-state
              (get :p)
              :l
              :my-node
              (assoc :nodeid :my-node)
              (update :time
                      #(-> %
                          coerce-time/to-long
                          (+ 3)
                          (* 1000)))
              (assoc :sensor "l-i")
              sut/handle-l-value!)
          (is (= {:my-node {:value 73
                            :time (pd/node-time-to-joda t2)
                            :sensor "l"}}
                 (-> @sut/profile-state
                     (get :p)
                     :l))))))))

(deftest calculate-driver-test
  (testing "`calculate-driver!` sends correct values to `dh/light`"
    (let [invocation-log (atom [])]
      (with-redefs [light-utils/get-now (constantly (pd/node-time-to-joda 0))
                    sut/profile-state (atom {:p {:active? true
                                                 :l {:node-a {:value 2
                                                              :time (pd/node-time-to-joda 0)}
                                                     :node-b {:value 8
                                                              :time (pd/node-time-to-joda 0)}
                                                     :node-c {:value 12
                                                              :time (pd/node-time-to-joda 0)}}}})
                    sut/get-etdhprofile (constantly {:etdhprofile test-etdhprofile})
                    sut/get-etdh-nodes (constantly {:triggers [:node-b
                                                               :node-c]
                                                    :controllers [:node-a
                                                                  :node-b
                                                                  :node-c]})
                    sut/schedule-next-poll! (constantly nil)
                    dh/light #(swap! invocation-log conj %&)]
        (sut/calculate-driver! :p)
        (is (= [[[:node-a :node-b :node-c] 80]]
               @invocation-log)))))
  (testing "`calculate-driver!` schedules `run-controller!`"
    (let [invocation-log (atom [])
          fast-poll 23
          schedule-args [:run-controller :p
                         (- fast-poll
                            sut/poll-response-timeout)
                         java.util.concurrent.TimeUnit/SECONDS]]
      (with-redefs [sut/profile-state (atom {:p {:active? true
                                                 :fsm {:state :fast-polling}}})
                    sut/get-etdhprofile (constantly {:etdhprofile {:fast-poll fast-poll}})
                    sut/schedule-shim #(do
                                         (swap! invocation-log conj [:schedule %&])
                                         [:schedule-callback %&])
                    sut/track-mean-lux! (constantly nil)
                    sut/calculate-driver-level (constantly nil)]
        (sut/calculate-driver! :p)
        (is (= [[:schedule schedule-args]]
               @invocation-log))
        (is (= [:schedule-callback schedule-args]
               (-> @sut/profile-state
                   :p
                   :run-controller))))))
  (testing "`calculate-driver!` calls `track-mean-lux!`"
    (let [p (promise)]
      (with-redefs [light-utils/get-now (constantly (pd/node-time-to-joda 0))
                    sut/profile-state (atom {:p {:active? true
                                                 :l {:t {:value 1
                                                         :time (pd/node-time-to-joda 0)}}}})
                    sut/get-etdh-nodes (constantly {:triggers [:t]})
                    sut/get-etdhprofile (constantly {:etdhprofile test-etdhprofile})
                    sut/track-mean-lux! (fn [& _]
                                          (deliver p :ok))
                    sut/calculate-driver-level (constantly nil)
                    sut/schedule-next-poll! (constantly nil)]
        (sut/calculate-driver! :p))
      (is (= :ok
             (deref p
                    500 :not-ok))))))

(deftest track-mean-lux-test
  (testing "`track-mean-lux!` records prior values, trims but-last entry >10 minutes old, sends `dim-light`/`undim-light` FSM events"
    (let [invocation-log (atom [])
          fake-time (atom 0)]
      (with-redefs [light-utils/get-now #(-> @fake-time
                                             (* 1e3)
                                             long
                                             coerce-time/from-long)
                    sut/handle-fsm-event! #(swap! invocation-log conj %&)
                    sut/get-etdhprofile (constantly {:etdhprofile test-etdhprofile})
                    sut/get-etdh-nodes (constantly {:controllers [:node-a
                                                                  :node-b]
                                                    :triggers [:node-a
                                                               :node-b]})
                    sut/profile-state (atom {:p {:l {:node-a {:value 40
                                                              :time 12}
                                                     :node-b {:value 60
                                                              :time 20}
                                                     :node-c {:value 50
                                                              :time 2}}}})]
        (is (= []
               @invocation-log))

        (sut/track-mean-lux! :p 100)
        (is (= [[:p :undim-light]]
               @invocation-log))
        (is (= [100]
               (-> @sut/profile-state
                   (get :p)
                   :mean-lux
                   (->> (map :value)))))

        (swap! fake-time + 590)
        (sut/track-mean-lux! :p 9)
        (is (= [[:p :undim-light]]
               @invocation-log))
        (is (= [100
                9]
               (-> @sut/profile-state
                   (get :p)
                   :mean-lux
                   (->> (map :value)))))

        (swap! fake-time + 20)
        (sut/track-mean-lux! :p 10)
        (is (= [[:p :undim-light]
                [:p :dim-light]]
               @invocation-log))
        (is (= [100
                9
                10]
               (-> @sut/profile-state
                   (get :p)
                   :mean-lux
                   (->> (map :value)))))

        (swap! fake-time + 581)
        (swap! sut/profile-state update
               :p assoc
               :late? true)
        (sut/track-mean-lux! :p 8)
        (is (= [[:p :undim-light]
                [:p :dim-light]
                [:p :dim-light-and-late]]
               @invocation-log))
        (is (= [9
                10
                8]
               (-> @sut/profile-state
                   (get :p)
                   :mean-lux
                   (->> (map :value)))))))))

(deftest handle-fsm-times-test
  (testing "`handle-fsm-times!` calls intended fsm event"
    (let [invocation-log (atom [])]
      (with-redefs [sut/profile-state (atom {:p {}})
                    sut/handle-fsm-event! #(swap! invocation-log conj %&)]
        (sut/handle-fsm-times! :p :sunset-90)
        (sut/handle-fsm-times! :p :sunrise-90)
        (sut/handle-fsm-times! :p :midnight)
        (is (= [[:p :sunset-90]
                [:p :sunrise-90]
                [:p :midnight]]
               @invocation-log)))))

  (testing "`handle-fsm-times!` calls intended fsm event"
    (with-redefs [sut/profile-state (atom {:p {}})
                  sut/handle-fsm-event! (constantly nil)]
      (is (= nil
             (-> @sut/profile-state
                 :p
                 :late?)))

      (sut/handle-fsm-times! :p :midnight)
      (is (= true
             (-> @sut/profile-state
                 :p
                 :late?)))

      (sut/handle-fsm-times! :p :sunrise-90)
      (is (= false
             (-> @sut/profile-state
                 :p
                 :late?))))))

(deftest handle-trgl-value-test
  (testing "`handle-trgl-value!`"
    (let [invocation-log (atom [])]
      (with-redefs [sut/profile-state (atom {:p {}})
                    sut/get-profileid-from-trigger-node (constantly :p)
                    sut/get-etdh-nodes {:triggers [:my-node]}
                    light-utils/get-now (constantly (coerce-time/from-long 0))
                    sut/handle-fsm-event! #(swap! invocation-log conj %&)]
        (sut/handle-trgl-value! {:nodeid :my-node
                                 :value 1
                                 :time (* 1000
                                          1)})
        (is (= [[:p :low-trigger]]
               @invocation-log))


        (sut/handle-trgl-value! {:nodeid :my-node
                                 :value 0
                                 :time (* 1000
                                          2)})
        (is (= [[:p :low-trigger]
                [:p :high-trigger]]
               @invocation-log))))))

;; #4
(deftest etdh-config-test
  (let [high-lux 100
        low-lux 50]
    (with-redefs [sut/get-config-map (constantly {{:configid :old-configid} :etdh-configid})
                  sut/get-etdhprofile (constantly {:etdhprofile {:high-lux high-lux
                                                                 :low-lux low-lux}})]
      (is (= nil
             (sut/etdh-config :etdhprofileid {:configid :etdh-configid})))
      (is (= {:etdh-configid :etdh-configid}
             (sut/etdh-config :etdhprofileid {:configid :old-configid})))
      (is (= {:new-config {:lctrl_trigger_offt (* 1000
                                                  high-lux)
                           :lctrl_trigger_ont (* 1000
                                                 low-lux)}}
             (sut/etdh-config :etdhprofileid {:configid :new-configid}))))))

(deftest etdh-set-config-test
  (with-redefs [sut/->ETDHConfig (constantly (atom {}))]
    (sut/set-etdh-config! :etdhprofileid :old-configid :new-configid)
    (is (= {:old-configid :new-configid}
           (sut/get-config-map :etdhprofileid)))))

(deftest etdh-config-cape-test
  (with-redefs [sut/profile-state (atom {})
                sut/casel-requests (async/chan)
                sut/casel-responses (async/chan)]
    (cape/etdh-requests-consumer)
    (try
      (let [orgid (create-test-org)
            siteid (create-test-site orgid)
            etdhprofileid (create-test-etdhprofile orgid siteid)
            {:keys [model]} default-test-node
            config-nodeList (repeatedly 2 #(do
                                             {:nodeid (create-test-node orgid siteid)
                                              :configid (atom (uuid))}))
            lg (create-test-lighting-group orgid
                                           siteid
                                           {:nodeList (map :nodeid
                                                           config-nodeList)})]

        ;; Create two configs for the two nodes in the site, and then assign
        ;; one to each.
        (let [{:keys [success
                      config]} (run {:type "getDefaultConfigs"
                                     :user *user*
                                     :nodeprops {:model model}})
              config (assoc config
                            :model model)]
          (is (= true
                 success))
          (doseq [{:keys [configid
                          nodeid]} config-nodeList
                  :let [configprops (assoc config
                                           :configid @configid)]]
            (let [{:keys [success]} (run {:type "createConfig"
                                          :user *user*
                                          :orgprops {:orgid orgid}
                                          :siteprops {:siteid siteid}
                                          :configprops configprops})]
              (is (= true
                     success)))
            (let [{:keys [success]} (run {:type "applyConfigToNodes"
                                          :user *user*
                                          :orgprops {:orgid orgid}
                                          :siteprops {:siteid siteid}
                                          :configprops {:configid @configid}
                                          :nodeprops {:nodeids [nodeid]}})]
              (is (= true
                     success))))
          ;; Assert we have the configs we just assigned
          (doseq [{:keys [configid
                          nodeid]} config-nodeList]
            (is (= @configid
                   (-> (cape/get-config-for-node nodeid model)
                       :configid)))))

        ;; Now assign the nodes to a ETDHProfile, effectively changing
        ;; their assigned configs in order to be configured for ETDH.
        (let [{:keys [success]} (crud-test/apply-etdh-groups orgid
                                                             siteid
                                                             etdhprofileid
                                                             [lg])]
          (is (= true
                 success)))

        ;; Confirm the ETDH Profile state contains the mapping for these
        ;; config changes.
        (let [config-map (sut/get-config-map etdhprofileid)]
          ;; We should have each of the original `configid`s as keys in
          ;; the map.
          (is (= (set (map (comp deref
                                 :configid)
                           config-nodeList))
                 (-> config-map
                     keys
                     (->> (map :configid))
                     set)))
          ;; Each Node should use the ETDH version of the config, as
          ;; specified in the map.
          (doseq [{:keys [configid
                          nodeid]} config-nodeList]
            (let [etdh-config (get config-map {:configid @configid
                                               :name (str "default_" model)})]
              (is (= etdh-config
                     (-> (cape/get-config-for-node nodeid model)
                         :configid)))
              (reset! configid etdh-config))))

        ;; Confirm when updating a ETDHProfile, a Config update is sent
        ;; to Nodes.
        (let [{:keys [success
                      etdhprofile]} (run {:type "updateETDHProfile"
                                          :user *user*
                                          :orgprops {:orgid orgid}
                                          :siteprops {:siteid siteid}
                                          :etdhprofileprops (-> default-test-etdhprofile
                                                                (assoc :etdhprofileid etdhprofileid)
                                                                (update :high-lux dec))})]
          (is (= true
                 success))
          (is (= (-> default-test-etdhprofile
                     sut/cleanup-etdhprofile
                     (assoc :etdhprofileid etdhprofileid
                            :sites []
                            :groups [{:groupid lg
                                      :name "Test Lighting Group"}]
                            :nodes (mapv #(select-keys % [:nodeid])
                                         config-nodeList))
                     (update :high-lux dec)
                     (update :scheduled vec)
                     vectors-to-sets)
                 (-> etdhprofile
                     vectors-to-sets))))

        (let [config-map (sut/get-config-map etdhprofileid)]
          ;; We should have each of the original `configid`s as keys in
          ;; the map.
          (is (= true
                 (clojure.set/subset? (set (map (comp deref
                                                      :configid)
                                                config-nodeList))
                                      (-> config-map
                                          keys
                                          (->> (map :configid))
                                          set))))
          ;; Each Node should use the ETDH version of the config, as
          ;; specified in the map.
          (doseq [{:keys [configid
                          nodeid]} config-nodeList]
            (let [etdh-config (->> config-map
                                   (filter (fn [[m _]]
                                                (= @configid
                                                   (:configid m))))
                                   first
                                   second)]
              (is (= etdh-config
                     (-> (cape/get-config-for-node nodeid model)
                         :configid)))
              (reset! configid etdh-config)))))
      (finally
        (async/close! sut/casel-requests)))))
