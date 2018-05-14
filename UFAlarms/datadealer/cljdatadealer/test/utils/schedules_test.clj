(ns utils.schedules-test
  (:require [clj-time.core :as time-core]
            [clojure
             [test :refer :all]
             [walk :refer [keywordize-keys stringify-keys]]]
            [clojure.data.json :as json]
            [com.stuartsierra.component :as component]
            [dealer
             [devsvcctrl :as dev-ctrl]
             [devsvcctrl-test :as dev-ctrl-test]]
            [mqtt.fixture :as mqtt]
            [neowrap.neowrapper :as neo4j]
            [kafka.mock :as kafka]
            [clojure.tools.logging :refer :all]
            [utils
             [cape-test :refer [vectors-to-sets
                                default-test-no-network-schedule
                                run
                                *user*
                                create-test-org
                                create-test-site
                                create-test-node
                                create-test-schedule]]
             [cassandra_fixture :as cass]
             [neo4j-fixture :as neo4j-fixture]
             [schedules :as impl :refer :all]
             [solartime :as sol]])
  (:import org.joda.time.DateTime))

(use-fixtures :once (join-fixtures [cass/cassandra-fixture
                                    neo4j-fixture/neo4j-fixture
                                    kafka/kafka-fixture
                                    mqtt/mqtt-fixture
                                    dev-ctrl-test/device-service-control-fixture
                                    utils.cape-test/mock-google-api]))

(deftest priorities-test
  (is (= 6
         (count (set priorities)))))

(defn hms->seconds
  "Let's normalize to seconds for easy time comparisons."
  [{:keys [hr min sec]}]
  (+ sec
     (* 60 min)
     (* 60 60 hr)))

(defn before-or-at?
  "Could be optimized by implementing without a conversion to seconds
  for potentially *all* entries in the schedule computed for the
  Node. But, for now, is quite simple to understand."
  [now]
  (let [now (hms->seconds now)]
    (fn [then]
      (let [then (hms->seconds then)]
        (>= now then)))))

(defn pick-most-recent-schedule-slot
  "The Node's scheduler picks the most recent schedule slot."
  [time sorted-schedule]
  (->> sorted-schedule
       (take-while (before-or-at? time))
       last))

(defn get-most-recent-schedule-slot-on-loop
  "The Node's scheduler also loops around before midnight to find the
  most recent schedule slot."
  [time sorted-schedule]
  (or (pick-most-recent-schedule-slot time sorted-schedule)
      (last sorted-schedule)))

(def priority-skip-qualifier
  128)

(defn has-priority-skip-qualifier?
  [qualifiers]
  (= priority-skip-qualifier
     (bit-and priority-skip-qualifier qualifiers)))

(defn get-schedule-slot-wrt-qualifier
  "The Node's schedule picks the highest priority applicable. The next
  highest priority becomes available when the \"highest\" priority
  returns with qualifier 128."
  [time sorted-schedule]
  (->> sorted-schedule
       (partition-by :pri)
       (map #(get-most-recent-schedule-slot-on-loop time %))
       (remove (comp has-priority-skip-qualifier?
                     :qualifiers))
       first))

(defn mask-dates
  "For computed schedule slot entries with date attributes, see if
  those attributes exist for the current date-time we're trying to
  find a schedule slot for. If they do, compare them. If they match,
  keep them.

  This means entries with no date attributes will remain, unmolested."
  [time-input computed-schedule]
  (filter (fn [schedule-slot]
            (let [date-attributes #(->> (keys schedule-slot)
                                        (filter #{:wday :mday :mon :year})
                                        (select-keys %))]
              (->> [schedule-slot time-input]
                   (map date-attributes)
                   (apply =))))
          computed-schedule))

(defn emulate-node
  "Given a computed schedule, what schedule slots would the node
  choose?"
  [computed-schedule time-inputs]
  (let [computed-schedule (->> computed-schedule
                               (remove #(#{4} (:qualifiers %)))
                               (sort-by (juxt :pri :hr :min :sec)))]
    (reduce (fn [acc time-input]
              (->> computed-schedule
                   ;; A schedule *can* contain optional date
                   ;; attributes.
                   (mask-dates time-input)
                   (get-schedule-slot-wrt-qualifier time-input)
                   ((fn [{:keys [level]
                          :as slot}]
                      (assoc time-input
                             :slot slot
                             :node-level level)))
                   (conj acc)))
            []
            time-inputs)))

(deftest emulator-test-datemasked-empty-priority
  (is (= [{:hr 4, :min 4, :sec 4, :node-level 5,
           :slot {:qualifiers 0, :level 5, :pri 1, :hr 5, :min 5, :sec 5}}]
         (emulate-node [{:qualifiers 0 :level 5 :pri 1 :hr 5 :min 5 :sec 5}
                        {:qualifiers 0 :level 3 :pri 1 :hr 3 :min 3 :sec 3 :year 2015}]
                       [{:hr 4 :min 4 :sec 4}]))))

(defn get-actions-for-node
  [latitude longitude dt actions]
  (->> actions
       (map #(-> %
                 stringify-keys
                 (sol/convert-expression-to-timestamp
                  latitude
                  longitude
                  dt)))
       (sort-by :timestamp)
       (map (fn [{:keys [timestamp level]}]
              (->> [:hr :min :sec]
                   (map (juxt identity
                              #((% datetime-ops) timestamp)))
                   (into {:level level}))))))

(defn transform-API-datetimes
  "Take the API schedule expression and map the date-times to what the
  Node will use.

  Also, fix date for sunrise/set on the weekly?"
  [events latitude longitude dt]
  (let [events (keywordize-keys events)
        sol-util (position-sol-util latitude longitude)
        weekly (expand-weekly events sol-util dt)
        tz (sol/get-time-zone latitude longitude)
        dates (expand-daily events sol-util tz)]
    (->> (concat (apply concat (vals dates))
                 (apply concat (mapcat vals weekly)))
         (map #(dissoc % :qualifiers))
         set)))

(defn has-correct-light-level? [{:keys [level
                                        node-level]
                                 :as resolved-action}]
  "Given an action returned from the simulator, determine whether it
  is reporting the \"correct\" light level."
  (= level
     node-level))

(deftest test-parse-actions
  (let [latitude "37.380996"
        longitude "-121.992299"
        tz (sol/get-time-zone latitude longitude)
        dt (DateTime. 2015 1 7 1 10 0 0 tz)
        event (keywordize-keys
               {:date "2015-1-7"
                :actions [{"id" 3 "time" "sunrise+30" "level" 0}
                          {"id" 5 "time" "05:00:00"   "level" 100}
                          {"id" 4 "time" "23:00:00"   "level" 0}
                          {"id" 2 "time" "sunset"     "level" 100}]})

        result (->> (expand-daily [event] (position-sol-util latitude longitude) tz)
                    vals
                    first
                    (map #(dissoc % :year :mon :mday)))]

    (is (= []
           (->> (:actions event)
                (get-actions-for-node latitude longitude dt)
                (emulate-node result)
                (remove has-correct-light-level?))))))

(def default-no-network
  {:highTime "2:00:00"
   :highLevel 73})

(deftest test-parse-events

  (let [latitude "37.380996"
        longitude "-121.992299"
        events [{"days" ["sat" "sun"]
                 "actions" [{"id" 3 "time" "sunrise+30" "level" 0}
                            {"id" 5 "time" "05:00:00"   "level" 100}
                            {"id" 4 "time" "23:00:00"   "level" 0}
                            {"id" 2 "time" "sunset"     "level" 100}]}

                {"date" "2015-12-31"
                 "actions" [{"id" 3 "time" "sunrise"  "level" 0}
                            {"id" 2 "time" "sunset"   "level" 100}]}

                {"days" ["mon" "tue" "wed" "thu" "fri"]
                 "actions" [{"id" 3 "time" "sunrise+30" "level" 0}
                            {"id" 4 "time" "23:00:00"   "level" 50}
                            {"id" 5 "time" "05:00:00"   "level" 100}
                            {"id" 6 "time" "sunrise"    "level" 50}
                            {"id" 7 "time" "sunset-30"  "level" 50}
                            {"id" 2 "time" "sunset"     "level" 100}]}]

        tz (sol/get-time-zone latitude longitude)
        dt (DateTime. 2015 1 7 1 10 0 tz)]

    (is (= []
           (->> (transform-API-datetimes events latitude longitude dt)
                (emulate-node (parse-events latitude longitude events default-no-network dt))
                (remove has-correct-light-level?))))))

(deftest test-schedules-one

  (let [latitude "37.380996"
        longitude "-121.992299"
        events [{"days" ["mon" "tue" "wed" "thu" "fri"]
                 "actions" [{"id" 3 "time" "07:00:00" "level" 0}
                            {"id" 4 "time" "12:00:00" "level" 50}
                            {"id" 2 "time" "13:00:00" "level" 0}
                            {"id" 5 "time" "18:00:00" "level" 100}]}
                {"date" "2016-2-24"
                 "actions" [{"id" 2 "time" "sunrise"   "level" 50}
                            {"id" 3 "time" "09:00:00"  "level" 0}
                            {"id" 4 "time" "sunset"   "level" 50}
                            {"id" 5 "time" "20:00:00" "level" 100}]}]

        tz (sol/get-time-zone latitude longitude)
        dt (DateTime. 2015 1 7 1 10 0 tz)
        result (parse-events latitude longitude events default-no-network dt)]

    (is (= []
           (->> (transform-API-datetimes events latitude longitude dt)
                (emulate-node result)
                (remove has-correct-light-level?))))))

(deftest no-network-test
  (let [latitude "37.380996"
        longitude "-121.992299"
        events [{"days" ["sun" "mon" "tue" "wed" "thu" "fri" "sat"]
                 "actions" [{"id" 1 "time" "01:00:00" "level" 23}
                            {"id" 2 "time" "23:00:00" "level" 1}]}]
        tz (sol/get-time-zone latitude longitude)
        dt (DateTime. 2015 1 7 1 10 0 tz)
        no-network (assoc default-no-network
                          :lowTime "22:00:00"
                          :lowLevel 37)
        explicit-off-network-high (parse-events latitude longitude events default-no-network dt)
        explicit-off-network-high-and-low (parse-events latitude longitude events no-network dt)]

    (is (= (->> default-no-network
                ((juxt :highLevel :lowLevel))
                (remove nil?)
                set)
           (->> explicit-off-network-high
                (filter (comp #{4}
                              :qualifiers))
                (map :level)
                set)))
    (is (= (->> no-network
                ((juxt :highLevel :lowLevel))
                (remove nil?)
                set)
           (->> explicit-off-network-high-and-low
                (filter (comp #{4}
                              :qualifiers))
                (map :level)
                set)))))

(deftest test-new-resolved-actions

  (let [latitude "37.380996"
        longitude "-121.992299"
        tz (sol/get-time-zone latitude longitude)
        dt (DateTime. 2016 4 1 0 0 0 tz)
        single [{"time" "00:00:00" "level" 100}]
        basic [{"time" "sunrise" "level" 0}
               {"time" "sunset" "level" 100}]
        complex [{"time" "05:00:00"   "level" 100}
                 {"time" "sunrise"    "level" 50}
                 {"time" "sunrise+30" "level" 0}
                 {"time" "sunset-30"  "level" 50}
                 {"time" "sunset"     "level" 100}
                 {"time" "23:00:00"   "level" 50}]
        complex-2 [{"time" "10:00:00"   "level" 60}
                   {"time" "11:30:00"   "level" 20}
                   {"time" "11:40:00"   "level" 60}
                   {"time" "11:50:00"   "level" 0}]

        sol-util (position-sol-util latitude longitude)

        get-actions (fn [actions]
                      (let [events [(keywordize-keys
                                     {:date "2016-4-1"
                                      :actions actions})]]
                        (->> (expand-daily events sol-util tz)
                             vals
                             first
                             (map #(dissoc % :year :mon :mday)))))

        resultsSingle (get-actions single)
        resultsBasic (get-actions basic)
        resultsComplex (get-actions complex)
        resultsComplex2 (get-actions complex-2)]

    (is (= []
           (->> single
                (get-actions-for-node latitude longitude dt)
                (emulate-node resultsSingle)
                (remove has-correct-light-level?))))
    (is (= []
           (->> basic
                (get-actions-for-node latitude longitude dt)
                (emulate-node resultsBasic)
                (remove has-correct-light-level?))))
    (is (= []
           (->> complex
                (get-actions-for-node latitude longitude dt)
                (emulate-node resultsComplex)
                (remove has-correct-light-level?))))
    (is (= []
           (->> complex-2
                (get-actions-for-node latitude longitude dt)
                (emulate-node resultsComplex2)
                (remove has-correct-light-level?))))))

(deftest get-daily-schedule-test
  (let [schedule {:events [{:days ["sat" "sun"]
                            :actions [{"id" 3 "time" "sunrise+30" "level" 0}
                                      {"id" 5 "time" "05:00:00"   "level" 100}
                                      {"id" 4 "time" "23:00:00"   "level" 0}
                                      {"id" 2 "time" "sunset"     "level" 100}]}

                           {:date "2015-12-31"
                            :actions [{"id" 3 "time" "sunrise"  "level" 0}
                                      {"id" 2 "time" "sunset"   "level" 100}]}

                           {:days ["mon" "tue" "wed" "thu" "fri"]
                            :actions [{"id" 3 "time" "sunrise+30" "level" 0}
                                      {"id" 4 "time" "23:00:00"   "level" 50}
                                      {"id" 5 "time" "05:00:00"   "level" 100}
                                      {"id" 6 "time" "sunrise"    "level" 50}
                                      {"id" 7 "time" "sunset-30"  "level" 50}
                                      {"id" 2 "time" "sunset"     "level" 100}]}]
                  :network {:highTime "2:00:00"
                            :highLevel 73
                            :lowTime "22:00:00"
                            :lowLevel 37}}
        ;; The 27th is a Saturday, 29th is a Monday, and the 31st is
        ;; an explicitly separate "date" schedule.
        {:keys [latitude longitude]
         :as lat-lon} {:latitude "37.380996"
                       :longitude "-121.992299"}
        tz (sol/get-time-zone latitude longitude)
        dates (->> (range 27 32 2)
                   (map #(DateTime. 2015 12 % 0 0 0 tz)))]
    (is (= [6 8 4]
           (->> dates
                (map (comp count
                           #(get-daily-schedule schedule
                                                %
                                                lat-lon))))))))

(deftest get-daily-schedule-test2
  (let [schedule {:events [{:days ["mon"]
                            :actions [{"id" 1 "time" "sunrise+1" "level" 1}]}
                           {:days ["tue"]
                            :actions [{"id" 1 "time" "sunrise+30" "level" 2}]}
                           {:days ["wed"]
                            :actions [{"id" 1 "time" "sunrise+30" "level" 3}]}
                           {:days ["thu"]
                            :actions [{"id" 1 "time" "sunrise+30" "level" 4}]}
                           {:days ["fri"]
                            :actions [{"id" 1 "time" "sunrise+30" "level" 5}]}
                           {:days ["sat"]
                            :actions [{"id" 1 "time" "sunrise+30" "level" 6}]}
                           {:days ["sun"]
                            :actions [{"id" 1 "time" "sunrise+30" "level" 7}]}]
                  :network {:highTime "2:00:00"
                            :highLevel 73
                            :lowTime "22:00:00"
                            :lowLevel 37}}
        ;; The 27th is a Saturday, 29th is a Monday, and the 31st is
        ;; an explicitly separate "date" schedule.
        {:keys [latitude longitude]
         :as lat-lon} {:latitude "37.380996"
                       :longitude "-121.992299"}
        tz (sol/get-time-zone latitude longitude)
        dates (->> (range 7 14 1)
                   (map #(DateTime. 2015 12 % 12 0 0 tz)))]
    (is (= [1 2 3 4 5 6 7] (map #(.getDayOfWeek %) dates)))
    (is (= [1 2 3 4 5 6 7]
           (->> dates
                (mapv (comp
                      #(->> (get-daily-schedule schedule
                                                %
                                                lat-lon)
                            (filter (comp zero? :qualifiers))
                            first
                            :level))))))))

(deftest recently-midnight-test
  ;; There exist days where "midnight" as 12am/00:00:00 does not
  ;; exist due to daylight savings time.
  (is (= 1
         (-> (time-core/date-time 2016 10 16)
             .withTimeAtStartOfDay
             (time-core/from-time-zone (time-core/time-zone-for-id "America/Sao_Paulo"))
             time-core/hour)))

  (is (= ["Etc/GMT+8"]
         (-> (time-core/date-time 2016 01 01
                                  0 0 5)
             (time-core/from-time-zone (time-core/time-zone-for-id "America/Los_Angeles"))
             recently-midnight
             (->> (filter #(.startsWith % "Etc"))))))

  (is (= ["Etc/GMT+7"]
         (-> (time-core/date-time 2016 07 04
                                  0 0 5)
             (time-core/from-time-zone (time-core/time-zone-for-id "America/Los_Angeles"))
             recently-midnight
             (->> (filter #(.startsWith % "Etc")))))))

(deftest schedule-loop-test
  ;; What is midnight? It depends on where you are (because of time
  ;; zones) and what day of the year it is (because of daylights
  ;; savings).

  ;; Let's use Sunnyvale, Denver (one more timezone over), and
  ;; Phoenix -- which is also one more timezone over, but doesn't
  ;; subscribe to daylight savings -- half the year it's the same time
  ;; as Sunnyvale, the other half, it's the same time as Denver.

  ;; Use New Years, and 4th of July
  (try
    (let [orgid (create-test-org)
          sites [{:name "Sunnyvale, CA"
                  :latitude "37.371111"
                  :longitude "-122.0375"}
                 {:name "Denver, CO"
                  :latitude "39.76185"
                  :longitude "-104.881105"}
                 {:name "Phoenix, AZ"
                  :latitude "33.45"
                  :longitude "-112.066667"}]
          [sunnyvale
           denver
           phoenix
           :as site-schedule-nodes] (map (fn [site]
                                          (let [siteid (create-test-site orgid site)
                                                ;; HACK: Skip the following
                                                ;; once Peter's time-zone
                                                ;; application on create
                                                ;; lands in develop.
                                                cape-return (run {:type      "updateSite"
                                                                  :user      *user*
                                                                  :orgprops  {:orgid orgid}
                                                                  :siteprops (assoc site
                                                                                    :siteid siteid)})
                                                scheduleid (->> {:name (str "Test Schedule for "
                                                                            (:name site))}
                                                                (create-test-schedule orgid siteid))
                                                nodeid (->> {:name (str "Node for "
                                                                        (:name site))}
                                                            (create-test-node orgid siteid))]
                                            (assert (not (:error cape-return)))
                                            {:siteid siteid
                                             :scheduleid scheduleid
                                             :nodeid [nodeid]}))
                                        sites)

          new-years (-> (time-core/date-time 2016 01 01
                                             0 0 5)
                        (time-core/from-time-zone
                         (time-core/time-zone-for-id "America/Los_Angeles")))
          fourth-of-july (-> (time-core/date-time 2016 07 04
                                                  0 0 5)
                             (time-core/from-time-zone
                              (time-core/time-zone-for-id "America/Los_Angeles")))]

      (is (= [{:name "Sunnyvale, CA"
               :time_zone "America/Los_Angeles"}
              {:name "Denver, CO"
               :time_zone "America/Denver"}
              {:name "Phoenix, AZ"
               :time_zone "America/Phoenix"}]
             (->> site-schedule-nodes
                  (map (fn [{:keys [siteid]}]
                         (let [cape-return (run {:type "getSite"
                                                 :user *user*
                                                 :orgprops {:orgid orgid}
                                                 :siteprops {:siteid siteid}})]
                           (assert (not (:error cape-return)))
                           (-> cape-return
                               :site
                               (select-keys [:name
                                             :time_zone]))))))))

      (is (= ["Test Schedule for Sunnyvale, CA"
              "Test Schedule for Denver, CO"
              "Test Schedule for Phoenix, AZ"]
             (->> site-schedule-nodes
                  (map (fn [{:keys [siteid scheduleid]}]
                         (let [cape-return (run {:type "getSchedule"
                                                 :user *user*
                                                 :orgprops {:orgid orgid}
                                                 :siteprops {:siteid siteid}
                                                 :scheduleprops {:scheduleid scheduleid}})]
                           (assert (not (:error cape-return)))
                           (-> cape-return
                               :schedule
                               :name)))))))

      (is (= ["Node for Sunnyvale, CA"
              "Node for Denver, CO"
              "Node for Phoenix, AZ"]
             (->> site-schedule-nodes
                  (map (fn [{:keys [siteid nodeid]}]
                         (let [cape-return (run {:type "getNode"
                                                 :user *user*
                                                 :orgprops {:orgid orgid}
                                                 :siteprops {:siteid siteid}
                                                 :nodeprops {:nodeid (first nodeid)}})]
                           (assert (not (:error cape-return)))
                           (-> cape-return
                               :node
                               :name)))))))

      (->> site-schedule-nodes
           (map
            (fn [{:keys [siteid scheduleid nodeid]}]
              (let [cape-return (run {:type "applyScheduleToNode"
                                      :user *user*
                                      :orgprops  {:orgid orgid}
                                      :siteprops {:siteid siteid}
                                      :scheduleprops {:scheduleid scheduleid}
                                      :nodeprops {:nodeid (first nodeid)}})]
                (assert (not (:error cape-return))))))
           dorun)

      (let [r (atom nil)
            s (atom [])
            s-t-r schedules-to-recalculate]
        (with-redefs [schedules-to-recalculate (fn [& args]
                                                 (doto (apply s-t-r args)
                                                   (->> (reset! r))))
                      get-daily-schedule (constantly nil)
                      send-schedule #(swap! s conj %&)]
          (schedule-loop new-years)
          (is (= (map :nodeid [sunnyvale])
                 (map second @s)))
          (is (= [sunnyvale]
                 (map #(select-keys %
                                    [:siteid
                                     :scheduleid
                                     :nodeid])
                      @r)))

          (reset! r nil)
          (reset! s nil)
          (schedule-loop fourth-of-july)
          (is (= (->> [phoenix
                       sunnyvale]
                      (map :nodeid)
                      set)
                 (->> @s
                      (map second)
                      set)))
          (is (= #{phoenix
                   sunnyvale}
                 (->> @r
                      (map #(select-keys %
                                     [:siteid
                                      :scheduleid
                                      :nodeid]))
                      set))))))
    (finally
      (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;"))))

(deftest schedule-updater-test
  ;; Confirm that the `schedule-updater` will initialize, then call
  ;; `schedule-loop`, prefixed by `sleep-til-next-hour` each time.
  (let [sys (atom (component/system-map
                   :schedule-updater (new-schedule-updater)))
        ll (atom [])]
    (with-redefs [sleep-til-next-fifteen-minute-multiple (fn []
                                                           (swap! ll conj :sleep)
                                                           (Thread/sleep 50))
                  schedule-loop (fn [_]
                                  (swap! ll conj :update))]
      ;; Verify nothing happens until the thread is started
      (is (= []
             @ll))
      (swap! sys component/start)
      (Thread/sleep 130)
      ;; Confirm running thread is calling methods in expected order
      (is (= [:sleep :update
              :sleep :update
              :sleep]
             @ll))
      (swap! sys component/stop)
      (reset! ll [])
      ;; Confirm stopped system stops calling methods
      (Thread/sleep 70)
      (is (= []
             @ll)))))

(deftest new-crud-has-last-delivered-prop-test
  (try
    (let [org (create-test-org)
          site (create-test-site org)
          scheduleid (create-test-schedule org site)]
      (is (= {:when 0}
             (-> "MATCH (s:Schedule {scheduleid: {props}.scheduleid}) RETURN s.last_delivered AS when"
                 (neo4j/executeQuery (stringify-keys {:scheduleid scheduleid}))
                 (json/read-str :key-fn keyword)))))
    (finally
      (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;"))))

(deftest send-loginreq-schedule-test
  (try
    (let [orgid (create-test-org)
          site {:name "Sunnyvale, CA"
                :latitude "37.371111"
                :longitude "-122.0375"}
          {:keys [siteid scheduleid nodeid]
           :as sunnyvale} (let [siteid (create-test-site orgid site)
                                ;; HACK: Skip the following once
                                ;; Peter's time-zone application on
                                ;; create lands in develop.
                                cape-return (run {:type      "updateSite"
                                                  :user      *user*
                                                  :orgprops  {:orgid orgid}
                                                  :siteprops (assoc site
                                                                    :siteid siteid)})
                                scheduleid (->> {:name (str "Test Schedule for "
                                                            (:name site))}
                                                (create-test-schedule orgid siteid))
                                nodeid (->> {:name (str "Node for "
                                                        (:name site))}
                                            (create-test-node orgid siteid))]
                            (assert (not (:error cape-return)))
                            {:siteid siteid
                             :scheduleid scheduleid
                             :nodeid nodeid})]

      (is (= {:name "Sunnyvale, CA"
              :time_zone "America/Los_Angeles"}
             (let [cape-return (run {:type "getSite"
                                     :user *user*
                                     :orgprops {:orgid orgid}
                                     :siteprops {:siteid siteid}})]
               (assert (not (:error cape-return)))
               (-> cape-return
                   :site
                   (select-keys [:name
                                 :time_zone])))))

      (is (= "Test Schedule for Sunnyvale, CA"
             (let [cape-return (run {:type "getSchedule"
                                     :user *user*
                                     :orgprops {:orgid orgid}
                                     :siteprops {:siteid siteid}
                                     :scheduleprops {:scheduleid scheduleid}})]
               (assert (not (:error cape-return)))
               (-> cape-return
                   :schedule
                   :name))))

      (is (= "Node for Sunnyvale, CA"
             (let [cape-return (run {:type "getNode"
                                     :user *user*
                                     :orgprops {:orgid orgid}
                                     :siteprops {:siteid siteid}
                                     :nodeprops {:nodeid nodeid}})]
               (assert (not (:error cape-return)))
               (-> cape-return
                   :node
                   :name))))

      (let [node-payload (keywordize-keys (send-loginreq-schedule siteid scheduleid nodeid false nil))]
        (is (= [nodeid]
               (:nodeid node-payload)))
        (is (get #{6 8} ;; weekend has 4 slots, week has 6 -- add
                 ;; two to both for no-network schedule.
                 (count (:schedules node-payload))))))
    (finally
      (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;"))))

(deftest apply-schedule-test-photocell
  (try
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          nodeid (spy :debug (create-test-node orgid siteid))
          schedule {:events [{:days                ["mon" "tue" "wed" "thu" "fri" "sat" "sun"]
                              :photocell_enabled   true
                              :photocell_highLevel 100
                              :photocell_lowLevel  0
                              :actions             [{:time "00:00:00" :level 10}]}]}
          scheduleid (spy :debug (create-test-schedule orgid siteid schedule))
          node-payload (atom [])]
      (is (= {:success  true
              :schedule (-> (assoc schedule
                              :scheduleid scheduleid
                              :name "Test Schedule"
                              :description "This is a test schedule"
                              :network default-test-no-network-schedule
                              :sites []
                              :groups []
                              :nodes [{:nodeid nodeid}])
                            vectors-to-sets)}
             (with-redefs [dev-ctrl/query-exec-msgpk #(swap! node-payload conj %&)]
               (-> {:type          "applyScheduleToNode"
                    :user          *user*
                    :orgprops      {:orgid orgid}
                    :siteprops     {:siteid siteid}
                    :nodeprops     {:nodeid nodeid}
                    :scheduleprops {:scheduleid scheduleid}}
                   run
                   vectors-to-sets))))
      (spy :debug @node-payload))
  (finally
    (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;"))))

(deftest cape-apply-schedule-test
  (try
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          nodeid (create-test-node orgid siteid)
          schedule {:events [{:days                ["sat" "sun"]
                              :photocell_enabled   false
                              :photocell_highLevel 100
                              :photocell_lowLevel  0
                              :actions             [{:time "sunset" :level 100}
                                                    {:time "sunrise+30" :level 0}
                                                    {:time "23:00:00" :level 0}
                                                    {:time "05:00:00" :level 100}]}
                             {:days                ["mon" "tue" "wed" "thu" "fri"]
                              :photocell_enabled   false
                              :photocell_highLevel 100
                              :photocell_lowLevel  0
                              :actions             [{:time "sunset" :level 100}
                                                    {:time "sunrise+30" :level 0}
                                                    {:time "23:00:00" :level 50}
                                                    {:time "05:00:00" :level 100}
                                                    {:time "sunrise" :level 50}
                                                    {:time "sunset-30" :level 50}]}]}
          scheduleid (create-test-schedule orgid siteid schedule)
          node-payload (atom [])]

      (is (= {:success  true
              :schedule (-> (assoc schedule
                              :scheduleid scheduleid
                              :name "Test Schedule"
                              :description "This is a test schedule"
                              :network default-test-no-network-schedule
                              :sites []
                              :groups []
                              :nodes [{:nodeid nodeid}])
                            vectors-to-sets)}
             (with-redefs [dev-ctrl/query-exec-msgpk #(swap! node-payload conj %&)]
               (-> {:type          "applyScheduleToNode"
                    :user          *user*
                    :orgprops      {:orgid orgid}
                    :siteprops     {:siteid siteid}
                    :nodeprops     {:nodeid nodeid}
                    :scheduleprops {:scheduleid scheduleid}}
                   run
                   vectors-to-sets))))
      (spy :debug @node-payload)

      (is (->> @node-payload
               first
               first
               :schedules
               count
               (get #{6 8}))))

    (finally
      (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;"))))
