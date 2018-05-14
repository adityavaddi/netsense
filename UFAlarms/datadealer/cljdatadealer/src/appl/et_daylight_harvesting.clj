(ns ^{:doc "Event Triggered Daylight Harvesting"}
    appl.et-daylight-harvesting
  (:require [appl.lighting-control.utils :as light-utils]
            [appl.et-daylight-harvesting
             [fsm :as fsm]
             [spec :as etdh-spec]]
            [appl.daylight_harvest :as dh]
            [appl.proximity-dimming :as pd]
            [clj-time.core :as time-core]
            [clojure.core.async :as async]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.future :refer :all]
            [clojure.spec.gen.alpha :as gen]
            [clojure.core.match :refer [match]]
            [clojure.tools.logging :refer :all]
            [com.stuartsierra.component :as component]
            [dealer.devsvcctrl :as dev-ctrl]
            [dealer.metrics :as metrics]
            [metrics.timers :as timers]
            [neowrap.neowrapper :as neo4j]))

;; See: https://xeranet.atlassian.net/wiki/pages/viewpage.action?pageId=123945511
;;      https://xeranet.atlassian.net/wiki/pages/viewpage.action?pageId=122966762

(defonce metric-factory
  (metrics/metric-factory-stub "appl.proximity-dimming"))

(defn with-etdhprofile-defaults
  [profile]
  (merge {:min-lux etdh-spec/default-min-lux
          :high-driver etdh-spec/default-high-driver
          :fast-poll etdh-spec/default-fast-poll
          :slow-poll etdh-spec/default-slow-poll
          :scheduled etdh-spec/default-schedule}
         profile))

(defn cleanup-etdhprofile
  [profile]
  (-> profile
      with-etdhprofile-defaults
      (update :scheduled (comp distinct
                               vec))))

(defn explain-etdhprofile
  [etdhprofile]
  (when-let [problems (s/explain-data :etdh/etdhprofile
                                      (cleanup-etdhprofile etdhprofile))]
    (try
      (->> problems
           :clojure.spec.alpha/problems
           (map etdh-spec/parse-problem)
           distinct
           (interpose "\n")
           (apply str))
      (catch Throwable t
        (warn t)
        "Invalid input."))))

(defn get-etdhprofile
  [etdhprofileid]
  (with-open [_ (timers/start (metric-factory :timer
                                              ["profile-fetch"]))]
    (let [cypher-return (-> "cyphers/get_etdhprofile.cypher"
                            io/resource
                            slurp
                            (neo4j/executeQuery {"etdhprofileid" etdhprofileid})
                            (json/read-str :key-fn keyword))]
      (if (= {}
             cypher-return)
        (warnf "Unable to find ETDHProfile %s"
               etdhprofileid)
        (-> cypher-return
            (update-in [:etdhprofile :scheduled]
                       (fnil json/read-str "{}")
                       :key-fn keyword))))))

(defn slope-intercept
  [x1 y1 x2 y2]
  (let [slope (/ (- y2 y1)
                 (- x2 x1))
        ;; y = m*x + b
        ;; y - m*x = b
        b (- y1
             (* slope x1))]
    [slope b]))

(defn calculate-driver-level
  [{:keys [high-lux high-driver
           low-lux  low-driver
           min-lux  min-driver]}
   lux]
  {:pre [high-lux high-driver
         low-lux  low-driver
         min-lux  min-driver]}
  (let [[high-low-slope
         high-low-intercept] (slope-intercept high-lux high-driver
                                              low-lux  low-driver)
        [low-min-slope
         low-min-intercept]  (slope-intercept low-lux  low-driver
                                              min-lux  min-driver)]
    (int (cond
           (>= lux high-lux) high-driver
           (>  lux low-lux)  (-> lux
                                 (* high-low-slope)
                                 (+ high-low-intercept))
           (=  lux low-lux)  low-driver
           (>  lux min-lux)  (-> lux
                                 (* low-min-slope)
                                 (+ low-min-intercept))
           (<= lux min-lux)  min-driver))))

(defn get-config-map-
  [etdhprofileid]
  (let [{:keys [id
                configmap]} (-> "MATCH (p:ETDHProfile)
WHERE p.etdhprofileid={props}.etdhprofileid
RETURN {id: p.etdhprofileid, configmap: p.config}
AS ret"
                                (neo4j/executeQuery {"etdhprofileid" etdhprofileid})
                                (json/read-str :key-fn keyword)
                                :ret)]
    (if-not id
      (doto (ex-info "Could not retrieve configmap for non-existant ETDH Profile"
                     {:etdhprofileid etdhprofileid})
        warn
        throw)
      (if-not configmap
        {}
        (-> configmap
            (json/read-str :key-fn clojure.edn/read-string))))))

(defn reset-etdh-config-!
  [etdhprofileid newval]
  (-> newval
      (json/write-str :key-fn prn-str)
      (->> (assoc {"etdhprofileid" etdhprofileid}
                  "configmap")
           (neo4j/executeQuery "MATCH (p:ETDHProfile)
WHERE p.etdhprofileid={props}.etdhprofileid
SET p.config={props}.configmap")))
  newval)

(defn swap-etdh-config-!
  [etdhprofileid f & a]
  (let [configmap (get-config-map- etdhprofileid)]
    (if-not configmap
      (doto (ex-info "Could not update configmap for non-existant ETDH Profile"
                     {:etdhprofileid etdhprofileid})
        warn
        throw)
      (->> (apply f configmap a)
           (reset-etdh-config-! etdhprofileid)))))

(defrecord ETDHConfig [etdhprofileid]
  clojure.lang.IDeref
  (deref [this]
    (get-config-map- etdhprofileid))
  clojure.lang.IAtom
  (reset [this newval]
    (reset-etdh-config-! etdhprofileid newval)
    @this)
  (swap [this f]
    (swap-etdh-config-! etdhprofileid f))
  (swap [this f a1]
    (swap-etdh-config-! etdhprofileid f a1))
  (swap [this f a1 a2]
    (swap-etdh-config-! etdhprofileid f a1 a2))
  (swap [this f a1 a2 args]
    (apply swap-etdh-config-! etdhprofileid f a1 a2 args)))

(defmethod clojure.pprint/simple-dispatch ETDHConfig [o]
  ((get-method clojure.pprint/simple-dispatch clojure.lang.IPersistentMap) o))

(defn get-config-map
  [etdhprofileid]
  @(->ETDHConfig etdhprofileid))

(defn swap-etdh-config!
  [etdhprofileid f & a]
  (apply swap! (->ETDHConfig etdhprofileid) f a))

(defn set-etdh-config!
  [etdhprofileid config-key new-configid]
  (swap! (->ETDHConfig etdhprofileid) assoc
         config-key new-configid))

(defn etdh-config-payload
  [etdhprofileid]
  (-> (get-etdhprofile etdhprofileid)
      (update :etdhprofile
              (fn [{:keys [high-lux
                           low-lux]}]
                {:pre [high-lux
                       low-lux]}
                {:lctrl_trigger_offt (* 1000
                                        high-lux)
                 :lctrl_trigger_ont (* 1000
                                       low-lux)}))
      :etdhprofile))

(defn etdh-config
  [etdhprofileid
   {:keys [configid]
    :as config-key}]
  (let [config-map (get-config-map etdhprofileid)
        etdh-configids (set (vals config-map))]
    (cond
      ;; If the `configid` is already a config we've created for this
      ;; ETDH Profile, there's nothing left to do.
      (etdh-configids configid) nil
      ;; Otherwise, see if we've mapped this non-ETDH Profile Config
      ;; to a Config for this *specific* ETDH Profile.
      (get config-map config-key) {:etdh-configid (get config-map config-key)}
      ;; When the `configid` is neither a ETDH Profile Config, nor a
      ;; Config that has been mapped to an ETDH Profile Config, we
      ;; need to create one for this Profile.
      :else {:new-config (etdh-config-payload etdhprofileid)})))

(def casel-requests
  (async/chan))

(def casel-responses
  (async/chan))

(defn handle-casel
  [jsmap]
  (async/>!! casel-requests jsmap)
  (async/<!! casel-responses))

(def get-nodeids
  #(mapv :nodeid %))

(defn get-etdh-nodes
  [etdhprofileid]
  (let [query-results (-> "cyphers/get_etdhprofile_nodes.cypher"
                          io/resource
                          slurp
                          (neo4j/executeQuery {"etdhprofileid" etdhprofileid})
                          (json/read-str :key-fn keyword))]
    (-> query-results
        (update :controllers get-nodeids)
        (update :triggers get-nodeids))))

(defn in-schedule?
  [etdhprofileid]
  (-> (get-etdhprofile etdhprofileid)
      (set/rename-keys {:etdhprofile :profile})
      light-utils/resolve-symbolic-time
      light-utils/in-schedule))

(defonce scheduler
  (new java.util.concurrent.ScheduledThreadPoolExecutor 1))

(defonce schedule-channel
  (async/chan))

(defonce profile-state
  (atom {}))

(defn schedule-shim
  "Shim to be mocked in unit tests."
  [callback-keyword etdhprofileid offset timeunit]
  (let [callback #(async/put! schedule-channel [callback-keyword etdhprofileid])]
    (.schedule scheduler
               ^java.util.concurrent.Callable callback
               offset
               timeunit)))

(defn schedule!
  "To manage the many time dependent callbacks, use a Java stdlib (see
  `scheduler`). However, instead of an opaque function to execute,
  deliver a value to the `schedule-channel` which will be processed in
  `schedule-callback-handler`."
  [callback-keyword etdhprofileid offset timeunit]
  (let [scheduler-callback (schedule-shim callback-keyword
                                          etdhprofileid
                                          offset
                                          timeunit)]
    (swap! profile-state update
           etdhprofileid assoc
           callback-keyword scheduler-callback)))

(defn schedule-cancel-shim
  [schedule-ref]
  (.cancel schedule-ref true))

(defn schedule-cancel!
  [etdhprofileid timer]
  (when-let [schedule-ref (-> @profile-state
                              (get etdhprofileid)
                              timer)]
    (schedule-cancel-shim schedule-ref)
    (swap! profile-state update
           etdhprofileid dissoc
           timer)))

(defn millis-until-next-time-of-day
  [time-of-day {:keys [latitude longitude]
                :as coordinates}]
  {:pre [latitude
         longitude]}
  (let [now (light-utils/get-now)
        tod1 (light-utils/local-time->UTC time-of-day
                                          coordinates
                                          now)
        tod2 (light-utils/local-time->UTC time-of-day
                                          coordinates
                                          (time-core/plus now
                                                          (time-core/days 1)))
        next-tod (if (time-core/before? tod1
                                        now)
                   tod2
                   tod1)]
    (time-core/in-millis (time-core/interval now next-tod))))

(defn next-occuring-time
  [times coordinates]
  (let [n (light-utils/get-now)]
    (->> times
         ;; Calculate the offset for all candidate times.
         (map (juxt #(millis-until-next-time-of-day %
                                                    coordinates)
                    identity))
         ;; Sort by milliseconds
         sort
         first)))

(defn next-schedule
  [etdhprofileid]
  (let [{:keys [site]
         {:keys [scheduled]} :etdhprofile} (get-etdhprofile etdhprofileid)]
    (-> (mapcat vals scheduled)
        (next-occuring-time site))))

(defn schedule-next-boundary!
  [etdhprofileid]
  (with-open [_ (timers/start (metric-factory :timer
                                              ["schedule-next-boundary"]))]
    (let [[offset next-boundary-time] (next-schedule etdhprofileid)]
      (schedule! :check-schedule etdhprofileid
                 offset
                 java.util.concurrent.TimeUnit/MILLISECONDS))))

(def poll-response-timeout
  5)

(defn next-poll-interval
  [etdhprofileid]
  (let [{{:keys [fast-poll
                 slow-poll]} :etdhprofile} (get-etdhprofile etdhprofileid)
        {:keys [state]
         :as fsm} (-> @profile-state
                      (get etdhprofileid)
                      :fsm)]
    (when-let [poll (case state
                      :fast-polling fast-poll
                      :slow-polling slow-poll
                      :hour-polling 3600
                      :no-polling nil
                      (warn (ex-info "Unknown ETDH FSM state"
                                     {:state state
                                      :fsm fsm})))]
      (- poll
         poll-response-timeout))))

(defn schedule-next-poll!
  [etdhprofileid]
  (when-let [interval (next-poll-interval etdhprofileid)]
    (let [current-timer (-> @profile-state
                            (get etdhprofileid)
                            :run-controller)
          ;; Because we're not guaranteed to have an existing
          ;; `ScheduledFuture` value in `:run-controller`, we'll check
          ;; with a `when`.
          current-timer-value (when current-timer
                                (let [value (.getDelay current-timer java.util.concurrent.TimeUnit/SECONDS)]
                                  (when (pos? value)
                                    value)))
          ;; Due to an optional value, we need to choose a predicate
          ;; that, when `false`, means we need to schedule an
          ;; entry. The easiest way to do this is to wrap the predicate
          ;; for "Do we need to schedule an entry?" in a `not`.
          acceptable-current-timer? (when current-timer-value
                                      (not (> current-timer-value
                                              interval)))]
      (when-not acceptable-current-timer?
        (schedule! :run-controller etdhprofileid
                   interval
                   java.util.concurrent.TimeUnit/SECONDS)))))

(defn get-profileid-from-trigger-node
  [nodeid]
  (with-open [_ (timers/start (metric-factory :timer
                                              ["profileid-from-etdh-trigger"]))]
  (-> "cyphers/find_etdhprofile_for_trigger.cypher"
      io/resource
      slurp
      (neo4j/executeQuery {"nodeid" nodeid})
      (json/read-str :key-fn keyword)
      :profile
      first
      :etdhprofileid)))

(defn latest-value-update
  [{current-time :time
    :as current-value}
   {potential-time :time
    :as potential-value}]
  (if (or (nil? current-time)
          (time-core/before? current-time potential-time))
    potential-value
    current-value))

(defn prevent-l-to-l-i-downgrade
  [{current-sensor :sensor
    :as current-value}
   {potential-sensor :sensor
    :as potential-value}]
  (if (and (= "l" current-sensor)
           (= "l-i" potential-sensor))
    current-value
    (latest-value-update current-value
                         potential-value)))

(defn handle-l-value!
  [{:keys [time nodeid value]
    :as sample}]
  (when-let [etdhprofileid (get-profileid-from-trigger-node nodeid)]
    (when (-> @profile-state
              (get etdhprofileid)
              :active?)
      (swap! profile-state update-in
             [etdhprofileid :l nodeid] prevent-l-to-l-i-downgrade
             (-> sample
                 (select-keys [:value :time :sensor])
                 (update :time pd/node-time-to-joda)
                 (update :value int))))))

(defn handle-fsm-event!
  [etdhprofileid event]
  (if-not (contains? @profile-state etdhprofileid)
    (warn (ex-info "Cannot locate etdhprofileid"
                   {:etdhprofileid etdhprofileid
                    :event event}))
    (binding [fsm/*fsm-transition* (promise)]
      (swap! profile-state update-in
             [etdhprofileid :fsm] fsm/fsm-event event)
      (when (realized? fsm/*fsm-transition*)
        (schedule-next-poll! etdhprofileid)
        (when (= :fast-polling
                 @fsm/*fsm-transition*)
          ;; To avoid over-eager transitions to slow/hour polling,
          ;; truncate the `mean-lux` history.
          (swap! profile-state
                 update etdhprofileid
                 assoc :mean-lux
                 []))
        @fsm/*fsm-transition*))))

(defn handle-trgl-value!
  [{:keys [time nodeid value]
    :as sample}]
  (when-let [etdhprofileid (get-profileid-from-trigger-node nodeid)]
    (swap! profile-state update-in
           [etdhprofileid :trgl nodeid] latest-value-update
           (-> sample
               (select-keys [:value :time])
               (update :time pd/node-time-to-joda)
               (update :value int)))
    (let [one-minute-ago (time-core/minus (light-utils/get-now)
                                          (time-core/minutes 1))
          one-minute-ago-trgl (-> @profile-state
                                  (get etdhprofileid)
                                  :trgl
                                  vals
                                  (->> (remove (fn [{:keys [time]}]
                                                 (time-core/before? time
                                                                    one-minute-ago)))))
          majority-count (/ (count one-minute-ago-trgl)
                            2)
          {high 0
           low 1} (group-by :value one-minute-ago-trgl)]
      (when (> (count high)
               majority-count)
        (handle-fsm-event! etdhprofileid :high-trigger))
      (when (> (count low)
               majority-count)
        (handle-fsm-event! etdhprofileid :low-trigger)))))

(defn request-ambient-light
  [nodeids]
  (dev-ctrl/query-exec-msgpk {:nodeprops {:type "SensorSampleReq"
                                          :nodeid nodeids}
                              :extprops {:sensor "l-i"}})
  (dev-ctrl/query-exec-msgpk {:nodeprops {:type "SensorSampleReq"
                                          :nodeid nodeids}
                              :extprops {:sensor "l"}}))

(defn run-controller!
  [etdhprofileid]
  (cond
    (not (-> @profile-state
             (get etdhprofileid))) (warnf "Could not find ETDH Profile %s"
                                          etdhprofileid)
    (not (-> @profile-state
             (get etdhprofileid)
             :active?)) (warnf "Not polling ambient light from nodes for inactive ETDH Profile %s"
                               etdhprofileid)
    :else (do
            (let [{:keys [triggers]} (get-etdh-nodes etdhprofileid)
                  trigger-count (count triggers)]
              (if (zero? trigger-count)
                (do
                  (debugf "No trigger nodes for ETDH Profile %s"
                         etdhprofileid)
                  (schedule-next-poll! etdhprofileid))
                (do
                  (debugf "Polling %d nodes for ambient light for ETDH Profile %s"
                         trigger-count
                         etdhprofileid)
                  (request-ambient-light triggers)
                  (schedule! :calculate-driver etdhprofileid
                             poll-response-timeout
                             java.util.concurrent.TimeUnit/SECONDS)))))))

(defn track-mean-lux!
  [etdhprofileid mean-lux-sample]
  (let [{{:keys [min-lux]} :etdhprofile} (get-etdhprofile etdhprofileid)
        now (light-utils/get-now)
        ten-minutes-ago (time-core/minus now
                                         (time-core/minutes 10))
        {:keys [mean-lux
                late?]} (-> (swap! profile-state update-in
                                   [etdhprofileid :mean-lux]
                                   conj
                                   {:time now
                                    :value mean-lux-sample})
                            (get etdhprofileid))
        {old-means true
         new-means false} (group-by #(time-core/before? (:time %)
                                                        ten-minutes-ago)
                                    mean-lux)
        fsm-event (cond
                    (> mean-lux-sample min-lux) :undim-light
                    (empty? old-means) (debugf "Profile %s has not accrued over 10 minutes worth of samples since profile activation. Not considering transition to slow or hour polling."
                                              etdhprofileid)
                    (and late?
                         (->> new-means
                              (map :value)
                              (every? #(<= % min-lux)))) :dim-light-and-late
                    (->> new-means
                         (map :value)
                         (every? #(<= % min-lux))) :dim-light)]

    (when fsm-event
      (handle-fsm-event! etdhprofileid fsm-event))

    (swap! profile-state update-in
           [etdhprofileid :mean-lux]
           (fn [mean-lux]
             (let [{old true
                    new false} (group-by #(time-core/before? (:time %)
                                                             ten-minutes-ago)
                                         mean-lux)]
               (if (last old)
                 (vec (concat [(last old)] new))
                 new))))))

(defn calculate-driver!
  [etdhprofileid]
  (when (-> @profile-state
            (get etdhprofileid)
            :active?)
    (let [{:keys [etdhprofile]} (get-etdhprofile etdhprofileid)
          {:keys [triggers
                  controllers]} (get-etdh-nodes etdhprofileid)
          five-minutes-ago (time-core/minus (light-utils/get-now)
                                            (time-core/minutes 5))]
      (when etdhprofile
        (when-let [mean-lux (-> @profile-state
                                (get etdhprofileid)
                                :l
                                (select-keys triggers)
                                vals
                                (->> (remove (fn [{:keys [time]}]
                                               (time-core/before? time
                                                                  five-minutes-ago)))
                                     (map :value))
                                dh/iqm)]
          (track-mean-lux! etdhprofileid mean-lux)
          (->> mean-lux
               (calculate-driver-level etdhprofile)
               (dh/light controllers)
               (binding [dh/*dhprofileid* etdhprofileid])))
        (schedule-next-poll! etdhprofileid)))))

(defn schedule-fsm-times
  [etdhprofileid]
  (let [{:keys [site]} (get-etdhprofile etdhprofileid)]
    (doseq [[fsm-time time-string] {:sunset-90 "sunset-90"
                                    :sunrise-90 "sunrise-90"
                                    :midnight "00:00:00"}]
      (schedule! fsm-time etdhprofileid
                 (millis-until-next-time-of-day time-string site)
                 java.util.concurrent.TimeUnit/MILLISECONDS))))

(defn activate-profile!
  [etdhprofileid]
  (if-not (get @profile-state etdhprofileid)
    (warnf "Could not find ETDH Profile %s"
           etdhprofileid)
    (do
      (debugf "Activating ETDH Profile %s"
             etdhprofileid)
      (schedule-fsm-times etdhprofileid)
      (let [{:keys [site]} (get-etdhprofile etdhprofileid)
            late? (light-utils/in-schedule
                   (light-utils/resolve-symbolic-time {:site site
                                                       :profile {:beginTime "00:00:00"
                                                                 :endTime "sunrise-90"}}))]
        (swap! profile-state update
               etdhprofileid assoc
               :active? true
               :fsm (fsm/etdh-fsm {:etdhprofileid etdhprofileid})
               :late? late?))
      (run-controller! etdhprofileid))))

(defn deactivate-profile
  [profile]
  (-> profile
      (assoc :active? false)
      (dissoc :fsm
              :mean-lux
              :trgl)))

(defn deactivate-profile!
  [etdhprofileid]
  (if-not (get @profile-state etdhprofileid)
    (warnf "Could not find ETDH Profile %s"
           etdhprofileid)
    (do
      (debugf "Deactivating ETDH Profile %s"
             etdhprofileid)
      (let [{{:keys [nodes]} :etdhprofile} (get-etdhprofile etdhprofileid)]
        (light-utils/reset-lights (map :nodeid nodes)))
      (doseq [timer [:run-controller
                     :calculate-driver
                     :sunset-90
                     :sunrise-90
                     :midnight]]
        (schedule-cancel! etdhprofileid timer))
      (swap! profile-state update
             etdhprofileid deactivate-profile))))

(defn check-schedule!
  [etdhprofileid]
  (if-not (get @profile-state etdhprofileid)
    (warnf "Could not find ETDH Profile %s"
           etdhprofileid)
    (do
      (debugf "Checking schedule for %s"
             etdhprofileid)
      (if (in-schedule? etdhprofileid)
        (activate-profile! etdhprofileid)
        (deactivate-profile! etdhprofileid))
      (schedule-next-boundary! etdhprofileid))))

(defn add-profile!
  [etdhprofileid]
  (debugf "Adding ETDH Profile %s"
         etdhprofileid)
  (swap! profile-state assoc
         etdhprofileid {})
  (check-schedule! etdhprofileid))

(defn remove-profile!
  [etdhprofileid]
  (debugf "Removing ETDH Profile %s"
         etdhprofileid)
  (deactivate-profile! etdhprofileid)
  (doseq [timer [:check-schedule]]
    (schedule-cancel! etdhprofileid timer))
  (swap! profile-state dissoc
         etdhprofileid))

(defn update-profile!
  [user orgid siteid etdhprofileid]
  (debugf "Updating ETDHProfile %s"
         etdhprofileid)
  (let [config-map (get-config-map etdhprofileid)]
    (if (or (nil? config-map)
            (empty? config-map))
      (debugf "No config update needed; empty config map for %s"
             etdhprofileid)
      (doseq [[config-key old-configid] config-map]
        (let [{:keys [nodes]
               :as old-config} (-> "cyphers/get_config_by_id.cypher"
                                   io/resource
                                   slurp
                                   (neo4j/executeQuery {"configid" old-configid})
                                   (json/read-str :key-fn keyword)
                                   :config)
              new-config (etdh-config-payload etdhprofileid)]
          (cond
            (nil? old-config) (warnf "Could not fetch ETDH config %s"
                                     old-configid)
            (empty? nodes) (warnf "ETDH config %s has no Nodes."
                                  old-configid)
            (= new-config
               (select-keys old-config
                            (keys new-config))) (debugf "ETDH Config %s is unchanged."
                                                       old-configid)
            :else (do
                    (swap-etdh-config! etdhprofileid assoc
                                       config-key nil)
                    (handle-casel {:user user
                                   :type "applyETDHtoNodes"
                                   :orgprops {:orgid orgid}
                                   :siteprops {:siteid siteid}
                                   :etdhprofileprops {:etdhprofileid etdhprofileid}
                                   :nodeprops {:nodeids (map :nodeid nodes)}}))))))))

(defn handle-cape
  [cape-operation
   {:keys [user]
    {:keys [orgid]} :orgprops
    {:keys [siteid]} :siteprops
    {:keys [etdhprofileid]} :etdhprofileprops
    :as jsmap}]
  {:pre [(map? jsmap)]}
  (case cape-operation
    "createETDHProfile" (add-profile! etdhprofileid)
    "deleteETDHProfile" (remove-profile! etdhprofileid)
    "updateETDHProfile" (update-profile! user orgid siteid etdhprofileid)
    (warn (ex-info "Dropping message from CAPE"
                   jsmap))))

(defn handle-fsm-times!
  [etdhprofileid event]
  (if-not (contains? @profile-state
                     etdhprofileid)
    (warnf "Received FSM callback %s for non-existant profile %s"
           event
           etdhprofileid)
    (do
      (handle-fsm-event! etdhprofileid event)
      (case event
        :midnight (swap! profile-state update
                         etdhprofileid assoc
                         :late? true)
        :sunrise-90 (swap! profile-state update
                           etdhprofileid assoc
                           :late? false)
        nil))))

(defn schedule-callback-handler
  [[op profile]]
  (if-not (contains? @profile-state
                     profile)
    (warnf "Recieved schedule callback %s for non-existant profile %s"
           op
           profile)
    (do
      (schedule-cancel! profile op)
      (case op
        :check-schedule (check-schedule! profile)
        :run-controller (run-controller! profile)
        :calculate-driver (calculate-driver! profile)
        (:sunset-90
         :sunrise-90
         :midnight) (handle-fsm-times! profile op)
        (warnf "Unhandled schedule callback: %s"
               op)))))

(defn consume-schedule-channel!
  []
  (async/pipeline-blocking 1
                  (doto (async/chan) (async/close!))
                  (map (comp (constantly true)
                             schedule-callback-handler))
                  schedule-channel))

(defn get-all-etdhprofileids-with-site
  []
  (-> "cyphers/get_all_etdhprofileids_with_site.cypher"
      io/resource
      slurp
      neo4j/executeQuery
      (json/read-str :key-fn keyword)
      :items
      (->> (map :etdhprofileid))))

(defn startup!
  []
  (consume-schedule-channel!)
  (when-let [etdhprofileids (get-all-etdhprofileids-with-site)]
    (debugf "Startup found %d etdhprofiles."
            (count etdhprofileids))
    (doseq [etdhprofileid etdhprofileids]
      (add-profile! etdhprofileid))))

(defn shutdown!
  []
  (doseq [etdhprofileid (keys @profile-state)]
    (remove-profile! etdhprofileid)))

(defrecord ETDHScheduler [metrics-registry]
  component/Lifecycle
  (start [component]
    (info "Starting ETDH component.")
    (let [{:keys [registry
                  prefix]} metrics-registry]
      (alter-var-root #'metric-factory
                      (fn [& _]
                        (dealer.metrics/metric-factory-factory "appl.et-daylight-harvesting"
                                                               registry
                                                               prefix))))
    (startup!)
    component)
  (stop [component]
    (info "Stopping ETDH component.")
    (alter-var-root #'metric-factory
                    (fn [& _]
                      (metrics/metric-factory-stub "appl.proximity-dimming")))
    (shutdown!)
    component))

(defn new-etdh-scheduler
  []
  (map->ETDHScheduler {}))
