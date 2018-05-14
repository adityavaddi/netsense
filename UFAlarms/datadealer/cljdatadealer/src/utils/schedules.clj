(ns utils.schedules
  "For the Datadealer to accept API requests to set schedules on
  nodes, a translation, compilation, and optimization process is
  necessary."
  (:require [clj-time
             [coerce :as c]
             [core :as time-core]
             [format :as time-format]]
            [clojure.data.json :as json]
            [clojure.tools.logging :refer :all]
            [clojure.walk :refer [stringify-keys]]
            [clostache.parser :refer [render-resource]]
            [clojure.java.io :as io]
            [com.stuartsierra.component :as component]
            [dealer.devsvcctrl :as dev-ctrl]
            [kafka.producer :as kafka]
            [neowrap.neowrapper :as neo4j]
            [utils.solartime :as sol])
  (:import org.joda.time.DateTime))

;; A node schedule is a list of light levels with time expressions,
;; and optionally, parts of date expressions (year/month/day of
;; month/day of week). The node will find the most recent time
;; expression which isn't masked out by matching year/month/day/date.
;;
;; In addition, expressions are sorted into classes of priorities,
;; with qualifiers to search lower priorities. This precedence
;; mechanism allows various types of scoping to be possible. In
;; contrast to time expressions, qualifers are bitmasks, not specific
;; enums.
;;
;; Due to a limited number of available slots on the Node (10 on some,
;; 24 on others), it makes sense to exploit this scoping to avoid
;; unnecessary expansion (e.g., let a weekday schedule be a lower
;; priority, and have a weekend expand to the same schedule twice:
;; once for Saturday, once for Sunday).
;;
;; See:
;; - https://github.com/Xeralux/microx/blob/master/xwmapps/micronode/src/light/lctrl.c
;; - https://github.com/Xeralux/microx/blob/master/xwmapps/micronode/src/light/trigger.c
;; - https://github.com/Xeralux/microx/blob/master/xwmapps/micronode/src/util/calendar.c

(def priorities
  "The computed schedule uses four priorities, but for historical reasons has six."
  [1 2 6 7 8 9])

(def weekday-translation
  "Transform exposed API weekday expressions to Joda's
  representation."
  {"mon" 1
   "tue" 2
   "wed" 3
   "thu" 4
   "fri" 5
   "sat" 6
   "sun" 7})

(def datetime-ops
  "Lookup table for extracting DateTime properties the slot schedule
  expects to have."
  {:hr (memfn getHourOfDay)
   :min (memfn getMinuteOfHour)
   :sec (memfn getSecondOfMinute)
   :wday (memfn getDayOfWeek)
   :mday (memfn getDayOfMonth)
   :mon (memfn getMonthOfYear)
   :year (memfn getYear)})

(defn get-timestamp-properties
  "Utility function to extract Joda DateTime properties."
  [properties]
  {:pre [(every? (set (keys datetime-ops)) properties)]}
  (fn [timestamp]
    (if timestamp (map (juxt identity
                             #((% datetime-ops) timestamp))
                       properties))))

(defn photocell-actions [photocell_highLevel photocell_lowLevel]
  [{:level photocell_highLevel :time "00:00:00" :qualifiers 2}
   {:level photocell_lowLevel :time "00:00:00" :qualifiers 0}])

(defn maybe-photocell-actions [{:keys [photocell_enabled photocell_highLevel photocell_lowLevel actions] :as event}]
  (if photocell_enabled (photocell-actions photocell_highLevel photocell_lowLevel) actions))

(defn expand-weekly
  "Extract the weekly events and fully expand all the time entries,
  returning a sorted map of the day of the week to its slot schedules."
  [events sol-util dt]
  (let [weekly-ts-props (get-timestamp-properties [:wday
                                                   :hr :min :sec])]
    (->> events
         (filter :days)
         (map (fn [{:keys [days]
                    :as event}]
                (->> days
                     (map weekday-translation)
                     (map (fn [day]
                            [day (let [day (.withDayOfWeek dt day)
                                       actions (maybe-photocell-actions event)]
                                   (->> actions
                                        (map #(sol-util % day))
                                        (sort-by :timestamp)
                                        (map (fn [{:keys [timestamp level qualifiers] :or {qualifiers 0}}]
                                               (into {:level level
                                                      :qualifiers qualifiers}
                                                     (weekly-ts-props timestamp))))))]))
                     (into (sorted-map))))))))

(defn optimize-weekly-event
  "Given an expanded subweek, deduplicate where all days of the
  subweek have the same values. Then, return the slot count reduction
  savings."
  [event]
  (let [days (count event)
        actions (->> event
                     vals
                     (apply concat))
        n (count actions)
        common-attributes [:level :hr :min :sec :qualifiers]
        {duplicates true
         uniques false} (->> actions
                             (group-by (apply juxt common-attributes))
                             (group-by #(= days (count (second %)))))
        duplicates (->> duplicates
                        keys
                        (map #(->> %
                                   (map vector common-attributes)
                                   (into {:qualifiers 0}))))
        uniques (->> (vals uniques)
                     (apply concat))
        new-actions (concat duplicates uniques)]
    {:original event
     :optimized-actions new-actions
     :savings (- n (count new-actions))}))

(def previous-day-of-week
  (comp inc ; Move from 0-indexed to 1-indexed
        #(mod % 7) ; Handle the corner case
        dec ; To move to yesterday
        dec ; To move from 1-indexed to 0-indexed
        ))

(def next-day-of-week
  (comp inc
        #(mod % 7)))

;;;
;;; The necessity for these next two functions is a consequence of
;;; normalizing the user's submitted time to UTC (based on the
;;; latitude/longitude). This means "today" can be potentially split
;;; across two days when shifted to UTC.
;;;

(defn calculate-weekly-falling-edge-undims
  "The non-optimized subweeks (if any) are set to a priority above the
  optimized subweek. When the transitioning to the optimized subweek,
  we would like to coordinate a smooth handoff by targeting the
  earliest slot on that day."
  [weekly-maps falling-edges]
  (let [dow (keys weekly-maps)
        yesterdays (map previous-day-of-week
                        dow)]
    (->> dow
         (remove (set yesterdays))
         (map (comp #(update % :qualifiers + 128)
                    falling-edges
                    next-day-of-week)))))

(defn calculate-weekly-handoff-undims
  "Similar to `calculate-weekly-falling-edge-undims`, but when
  transitioning from the subweek, we want to permit a previous
  optimized day's slot to apply until it transitions to the next
  priority level."
  [weekly-maps]
  (let [dow (keys weekly-maps)
        yesterdays (map previous-day-of-week
                        dow)]
    (->> dow
         (remove (set yesterdays))
         (map (fn [dow]
                (let [tomorrow (next-day-of-week dow)]
                  (when (seq (filter #(= tomorrow (:wday %))
                                     (weekly-maps dow)))
                    {:qualifiers 128
                     :level 0
                     :wday tomorrow
                     :hr 0
                     :min 0
                     :sec 0
                     }))))
         (remove nil?))))

(defn date-parser
  [tz]
  {:pre [tz]}
  (fn [date-string]
    {:pre [date-string]}
    (-> (time-format/formatter "yyyy-MM-dd" tz)
        (time-format/parse date-string))))


(defn expand-daily
  "Similar to `expand-weekly`, expand the time entries, but at the
  date supplied in the event."
  [events sol-util tz]
  (let [date-ts-props (get-timestamp-properties [:year :mon :mday
                                                 :hr :min :sec])
        spyx (fn [x] (spy :error x))]
    (spyf :debug "expand-daily: %s" events)
    (spy :debug (->> events
         (filter :date)
         (map #(update % :date (date-parser tz)))
         (map (fn [{:keys [date actions]}]
                [date (->> actions
                           ;(spyx)
                           (map #(sol-util % date))
                           ;(spyx)
                           (sort-by :timestamp)
                           ;(spyx)
                           (map (fn [{:keys [timestamp level qualifiers] :or {qualifiers 0}}]
                                  (into {:level level :qualifiers qualifiers}
                                        (date-ts-props timestamp)))))]))
         (into (sorted-map))))))

(defn calculate-date-undims
  "Similar to `calculate-weekly-falling-edge-undims`. A specific date
  is given higher priority than a weekly entry, and so needs a
  transition to a weekly slot at the correct time."
  [date-specific falling-edges]
  (let [dates (keys date-specific)
        yesterdays (map #(time-core/minus % (time-core/days 1)) dates)
        date-props (get-timestamp-properties [:year :mon :mday])]
    (->> dates
         (remove (set yesterdays))
         (map (fn [date]
                (-> date
                    .getDayOfWeek
                    next-day-of-week
                    falling-edges
                    (update :qualifiers + 128)
                    (dissoc :wday)
                    (into (date-props date))))))))

(defn position-sol-util
  [latitude longitude]
  (fn [action date]
    (-> action
        clojure.walk/stringify-keys
        (sol/convert-expression-to-timestamp
         latitude
         longitude
         date))))

(defn calculate-off-network
  [dt
   sol-util
   tz
   {:keys [photocell_enabled
           photocell_highLevel
           photocell_lowLevel
           highTime
           highLevel
           lowTime
           lowLevel]
    :or {highTime "00:00:00"
         highLevel 100}}]
  (spyf :info "Off network values are: %s"
        [highTime
         highLevel
         lowTime
         lowLevel])
  (-> [{:date (->> dt
                   ((->> [:year :mon :mday]
                         (map datetime-ops)
                         (apply juxt)))
                   (interpose \-)
                   (apply str))
        :actions (clojure.walk/stringify-keys
                  (cond
                    photocell_enabled (photocell-actions photocell_highLevel photocell_lowLevel)
                    (and highTime
                         highLevel
                         lowLevel
                         lowTime) [{:level highLevel :time highTime}
                                   {:level lowLevel :time lowTime}]
                    (and highLevel
                         highTime) [{:level highLevel :time highTime}]
                    (and lowLevel
                         lowTime) [{:level lowLevel :time lowTime}]
                    :else []))}]
      (expand-daily sol-util tz)
      vals
      first))

(defn parse-events
  "Given the user events, expand the actions for weekly and for
  specific dates. Calculate the \"optimal\" weekly slot schedule for
  reduced slot usage. Calculate transitions from the non-reduced
  weekly schedule. Calculate transitions from specific dates. Throw in
  default \"off-network\" values. Order them and number."
  [latitude longitude events off-network dt]
  (when-not (or (empty? latitude)
                (empty? longitude))
    (let [tz (sol/get-time-zone latitude longitude)
          dt (DateTime. dt tz)

          sol-util (position-sol-util latitude longitude)

          events (clojure.walk/keywordize-keys events)

          weekly-map (spy :trace (expand-weekly events sol-util dt))

          falling-edges (->> weekly-map
                             (apply merge)
                             (map (fn [[dow slots]]
                                    [dow (first slots)]))
                             (into {}))

          date-specific (spy :trace (expand-daily events sol-util tz))
          date-undims (calculate-date-undims date-specific falling-edges)

          [{weekly-default :optimized-actions
            default-subweek :original}
           & weekly-maps] (->> weekly-map
                               (map optimize-weekly-event)
                               (sort-by :savings)
                               reverse
                               (spy :trace))

          weekly-maps (->> weekly-maps
                           (map :original)
                           (apply merge))

          weekly-undims (concat (calculate-weekly-falling-edge-undims weekly-maps falling-edges)
                                (calculate-weekly-handoff-undims default-subweek))

          off-network (calculate-off-network dt
                                             sol-util
                                             tz
                                             off-network)
          [p0a p0 p1a p1 p2 p3] priorities
          set-pri0 (fn [m] (if (not= 0 (bit-and (:qualifiers m) 2)) (assoc m :pri p0a) (assoc m :pri p0)))
          set-pri1 (fn [m] (if (not= 0 (bit-and (:qualifiers m) 2)) (assoc m :pri p1a) (assoc m :pri p1)))]

      (->> (concat (map #(-> %
                             (update :qualifiers + 4)
                             ;(assoc :pri p0)
                             (set-pri0)
                             (dissoc :year :mon :mday))
                        off-network)

                   ;; TODO: Cleanup the following thread-last
                   (->> (concat (map #(assoc % :pri p3)
                                     weekly-default)

                                (->> weekly-maps
                                     vals
                                     (apply concat)
                                     (concat weekly-undims)
                                     (map #(assoc % :pri p2))))

                        (group-by :wday)
                        ((fn [grouped-map]
                           (map #(get grouped-map %)
                                (conj (take 7
                                            (iterate next-day-of-week (.getDayOfWeek dt)))
                                      nil))))
                        (apply concat))

                   (->> date-specific
                        vals
                        (apply concat)
                        (concat date-undims)
                        (map set-pri1 ;#(assoc % :pri p1)
                             )))
           (spy :trace)
           (map-indexed (fn [i slot]
                          (assoc slot
                                 :id i
                                 :mask 1)))))))

(def daily-ts-props
  (get-timestamp-properties [:hr :min :sec]))

(defn get-daily-schedule
  "This function will get called daily at midnight, and once per node
  LoginReq (TODO: Implement a schedule-token?). Therefore, we only
  need to compute the schedule slots for the current day. This
  requires the API representation of the schedule (which is specified
  in site relative time), at date-time object to extract timezone for
  translating schedule slots times to UTC, and a lat-lon pair to
  calculate sunrise/sunset/timezones."
  [{:keys [events network]
    :as schedule}
   date-time
   {:keys [latitude longitude]}]
  {:pre [latitude longitude
         events network
         date-time]
   :post [;; We should *always* return schedule slots for any day of the week
          (seq %)]}
  ;(spy :error (format "get-daily-schedule: %s." schedule))
  (let [tz (sol/get-time-zone latitude longitude)
        date-time (time-core/minus (DateTime. (time-core/plus date-time (time-core/days 1)) tz) (time-core/days 1))
        dow (.getDayOfWeek date-time)
        event (or (->> events
                       (filter :date)
                       (filter (comp #(= (.toLocalDate date-time)
                                         %)
                                     #(.toLocalDate %)
                                     (date-parser tz)
                                     :date))
                       first)
                  (->> events
                       (filter :days)
                       (filter (comp #(some #{dow} %)
                                     #(map weekday-translation %)
                                     :days))
                       first))
        actions (maybe-photocell-actions event)
        sol-util (position-sol-util latitude longitude)
        spyx (fn [x] (spy :debug x))
        [p0a p0 p1a p1] priorities
        set-pri0 (fn [m] (if (not= 0 (bit-and (:qualifiers m) 2)) (assoc m :pri p0a) (assoc m :pri p0)))
        set-pri1 (fn [m] (if (not= 0 (bit-and (:qualifiers m) 2)) (assoc m :pri p1a) (assoc m :pri p1)))]
    (map-indexed (fn [i slot]
                   (assoc slot
                          :id i
                          :mask 1))
                 (concat
                  (->> network
                       ;(spyx)
                       (calculate-off-network date-time
                                              sol-util
                                              tz)
                       ;(spyx)
                       (map #(-> %
                                 (update :qualifiers + 4)
                                 ;(assoc :pri p0)
                                 set-pri0
                                 (dissoc :year :mon :mday))))
                  (->> actions
                       ;(spyx)
                       (map #(sol-util % date-time))
                       ;(spyx)
                       (sort-by :timestamp)
                       (map (fn [{:keys [timestamp level qualifiers] :or {qualifiers 0}}]
                              ;(spy :error (format "fn; level: %s, qualifiers: %s" level qualifiers))
                              (into {:level level
                                     :pri p1
                                     :qualifiers qualifiers}
                                    (daily-ts-props timestamp))))
                       (map set-pri1))))))

(defn send-schedule
  [schedule-slots nodes]
  (dev-ctrl/query-exec-msgpk {:nodeprops {:nodeid nodes
                                          :type "LightingScheduledEvent"}
                              :schedules schedule-slots}))

(defn recently-midnight
  "Return the timezone names which have recently become midnight."
  [datetime]
  (let [;Current time - 15 minutes
        minus-fifteen-min (time-core/minus datetime (time-core/minutes 15))
        ; Function which returns true if passed in time is between now and 15 minutes ago
        within-last-fifteen-min? #(time-core/within? minus-fifteen-min
                                                     datetime
                                                     %)
        ; Midnight start of day today
        beginning-of-day (.withTimeAtStartOfDay datetime)
        ; midnight start of day tomorrow
        beginning-of-tomorrow (.withTimeAtStartOfDay (time-core/plus datetime
                                                                     (time-core/days 1)))]
    ; For all of the timezone ids, filter them
    (->> (time-core/available-ids)
         (filter (fn [time-zone]
                   (let [; get timezone from timezone id
                         time-zone (time-core/time-zone-for-id time-zone)
                         ; get the beginning of the day and the beginning of tomorrow for the passed in timezone (so not in our timezone)
                         [beginning-of-day
                          beginning-of-tomorrow] (map #(time-core/from-time-zone % time-zone)
                                                      [beginning-of-day
                                                       beginning-of-tomorrow])]
                     ; If either of those datetimes are within 15 minutes of now, the filter function returns true, and they will be returned.
                     (or (within-last-fifteen-min? beginning-of-day)
                         (within-last-fifteen-min? beginning-of-tomorrow))))))))

(defn schedules-to-recalculate
  [datetime]
  (let [props (spy :debug (stringify-keys {:timezones (recently-midnight datetime)}))]
    (-> "MATCH (s:Site)-[:HAS]->(sch:Schedule)-[:HAS]->(n:Node)
WHERE s.time_zone in {props}.timezones AND n.model <> 'cnext'
WITH {
n: s.name,
siteid: s.siteid,
latitude: s.latitude,
longitude: s.longitude,
scheduleid: sch.scheduleid,
nodeid: collect(distinct(n.nodeid))
} AS items
RETURN collect(distinct(items)) AS items"
        (neo4j/executeQuery props)
        (json/read-str :key-fn keyword)
        :items)))

(defn send-daily-schedule
  [events network latitude longitude nodeids]
  {:pre [(seq nodeids)]}
  (-> (get-daily-schedule {:events events
                           :network network}
                          (time-core/now)
                          {:latitude latitude
                           :longitude longitude})
      (send-schedule nodeids)))

(defn send-schedule-to-lss [userid orgid siteid events network latitude longitude nodeids scheduleid]
  (let [request {:instanceid (.. java.net.InetAddress getLocalHost toString)
                 :timestamp (str (time-core/now))
                 :type "applySchedule"
                 :model "ScheduleModel"
                 :action "CAN_APPLY"
                 :user userid
                 :orgprops {:orgid orgid}
                 :siteprops {:siteid siteid}
                 :nodeprops {:nodeids nodeids}
                 :scheduleprops {:events events :network network :latitude latitude :longitude longitude :scheduleid scheduleid}}
        msg {:messageid (str (java.util.UUID/randomUUID))
             :request request}]
    (spy :debug msg)
    (kafka/send-to-producer @kafka/schedule-topic (json/write-str (stringify-keys msg)))))

(defn send-loginreq-schedule
  [siteid
   scheduleid
   nodeid
   send-to-lss?
   userid]
  (try
    (let [props (stringify-keys {:siteid siteid
                                 :nodeid nodeid})
          scheduleid (if (= "Unknown"
                            siteid)
                       "default"
                       scheduleid)
          lat-lon (if (= "default"
                         scheduleid)
                    {:latitude "0"
                     :longitude "0"}
                    (->
                     (doto
                         (-> "MATCH (s:Site {siteid: {props}.siteid})
                     RETURN {
                     latitude: s.latitude,
                     longitude: s.longitude
                     } AS ret"
                             (neo4j/executeQuery props)
                             (json/read-str :key-fn keyword))
                       (-> :exception not assert))
                     :ret))
          datetime (time-core/now)
          scht (-> "templates/send_assigned_schedule_to_node.cypher.tmpl"
                   (render-resource)
                   (neo4j/executeQuery {"scheduleid" scheduleid})
                   (json/read-str :key-fn keyword)
                   :schedule
                   (select-keys [:events
                                 :network]))
          schedule (-> scht
                       (get-daily-schedule datetime
                                           lat-lon))]
      ;(spyf :error "send-loginreq-schedule: %s" scht)
      ;(spy :error lat-lon)
      (if send-to-lss?
        (send-schedule-to-lss userid "Unknown" siteid (:events scht) (:network scht) (:latitude lat-lon) (:longitude lat-lon) [nodeid] scheduleid)
        (send-schedule schedule [nodeid])))
    (catch Throwable e
      (error {:siteid siteid
              :scheduleid scheduleid
              :nodeid nodeid})
      (error e))))

(defn get-nodes-model [nodeids]
  (spy :debug (-> "cyphers/get_nodes_model.cypher"
                  io/resource
                  slurp
                  (neo4j/executeQuery {"nodeids" nodeids})
                  (json/read-str :key-fn keyword)
                  (get :nodes))))

(defn send-midnight-schedule-to-lss []
  (let [request {:instanceid (.. java.net.InetAddress getLocalHost toString)
                 :timestamp (str (time-core/now))
                 :type "scheduleTrigger"
                 :model "ScheduleModel"
                 :extprops {:time (str (time-core/now))}}
        msg {:messageid (str (java.util.UUID/randomUUID))
             :request request}]
    (spy :debug msg)
    (kafka/send-to-producer @kafka/trigger-topic (json/write-str (stringify-keys msg)))))

(defn schedule-loop
  [datetime]
  (info "Updating schedules for sites at midnight.")
  ; Send message to LSS
  (send-midnight-schedule-to-lss)
  (->> (schedules-to-recalculate datetime)
       (spy :debug)
       (map (fn [{:keys [siteid
                         scheduleid
                         nodeid]
                  :as ssn}]
              (try
                (debugf "Recalculating schedule: %s" (dissoc ssn :nodeid))
                (let [lat-lon (select-keys ssn
                                           [:latitude
                                            :longitude])
                      schedule (-> "cyphers/get_schedule.cypher"
                                   io/resource
                                   slurp
                                   (neo4j/executeQuery {"scheduleid" scheduleid})
                                   (json/read-str :key-fn keyword)
                                   :schedule
                                   (select-keys [:events
                                                 :network])
                                   (get-daily-schedule datetime
                                                       lat-lon))]
                  (debugf "Recalculated schedule for %s nodes" (count nodeid))
                  ;; TODO: Introduce a jitter to not broadcast for all
                  ;; nodes at once, but in groups over a timespan.
                  (send-schedule schedule
                                 nodeid)
                  (-> "MATCH (s:Schedule {scheduleid: {props}.scheduleid})
                       SET s.last_sent = {props}.now"
                      (neo4j/executeQuery {"scheduleid" scheduleid
                                           "now" (str datetime)})))
                (catch Exception e
                  (error e)))))
       dorun))

(def fifteen-minute-offsets
  [0 15 30 45])

(defn sleep-til-next-fifteen-minute-multiple
  []
  (let [now (time-core/now)
        plus-fifteen-minutes (time-core/plus now (time-core/minutes 15))
        rounded-minute (nth fifteen-minute-offsets
                            (quot (time-core/minute plus-fifteen-minutes)
                                  15))
        next-hour (-> plus-fifteen-minutes
                      ((juxt time-core/year
                             time-core/month
                             time-core/day
                             time-core/hour))
                      (conj rounded-minute)
                      (->> (apply time-core/date-time)
                           (spyf :info "Sleeping until: %s")))]
    (->> next-hour
         (time-core/interval now)
         time-core/in-millis
         Thread/sleep)
    (infof "Sleep until %s finished." next-hour)))

(defn schedule-updater
  []
  (try
    (info "Starting the schedule updater.")
    ;; If a schedule *hasn't* been updated "recently", it doesn't
    ;; matter -- we always recalculate the schedule for a node on
    ;; LoginReq.
    (sleep-til-next-fifteen-minute-multiple)
    (loop []
      (schedule-loop (time-core/now))
      (sleep-til-next-fifteen-minute-multiple)
      (recur))
    (catch java.lang.InterruptedException e
      (info "Schedule updater is ending."))
    (catch Exception e
      (error e))))

(defrecord ScheduleUpdater [schedule-updater-thread]
  component/Lifecycle
  (start [component]
    (let [thread (Thread. schedule-updater)]
      (info "Starting schedule updater in a thread.")
      (.start thread)
      (assoc component
             :schedule-updater-thread thread)))
  (stop [component]
    (info "Stopping schedule updater.")
    (when schedule-updater-thread
      (.interrupt schedule-updater-thread))
    (dissoc component :schedule-updater-thread)))

(defn new-schedule-updater
  []
  (map->ScheduleUpdater {}))

(defn validate-daily-schedule
  "This function verifies max number of slots for daily schedules.
  It's called when verifying createSchedule and updateSchedule properties"
  [scheduleprops siteprops]
  (let [events         (:events scheduleprops)
        network        (:network scheduleprops {})
        siteid         (:siteid siteprops)
        lat-lon        (if (= "Unknown"
                              siteid)
                           {:latitude "0" :longitude "0"}
                           (->
                             (doto
                               (-> "MATCH (s:Site {siteid: {props}.siteid})
                                     RETURN {
                                     latitude: s.latitude,
                                     longitude: s.longitude
                                     } AS ret"
                                   (neo4j/executeQuery {"siteid" siteid})
                                   (json/read-str :key-fn keyword))
                               (-> :exception not assert))
                             :ret))
        tz             (sol/get-time-zone (:latitude lat-lon) (:longitude lat-lon))
        dates          (->> events
                            (filter :date)
                            (map (fn [event] (:date event)))
                            sort
                            (map (fn [dt] (DateTime. dt tz)))) ;; All dates defined in events
        mindate        (if (empty? dates) (time-core/now) (first dates))
        days           (take 7 (iterate (fn [dt]
                                          (time-core/minus dt (time-core/days 1))
                                          )
                                        (time-core/minus mindate (time-core/days 1))))  ;; 7 days not found in dates
        dates_and_days (spy (into (spy :trace dates) (spy :trace  days))) ;; All dates defined in events + 7 previous days
        result         (->> dates_and_days
                            (map (fn [dt] (get-daily-schedule {:events events
                                                               :network network}
                                                              dt
                                                              lat-lon)))
                            (filter (fn [slots] (> (count slots) 8))))  ;; Find all with more than 8 slots
        ]
    (empty? (spy :trace result))))
