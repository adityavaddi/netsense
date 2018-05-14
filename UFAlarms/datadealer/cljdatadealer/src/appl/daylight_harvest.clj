(ns ^{:doc "Daylight Harvesting Application."}
    appl.daylight_harvest
  (:gen-class)
  (:refer-clojure :exclude [iterate])
  (:require [clj-time.core :as time-core]
            [clojure.core.async :as async]
            [appl.lighting-control.utils :as light-utils]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.tools.logging :refer :all]
            [clojure.walk :refer [keywordize-keys stringify-keys]]
            [clostache.parser :refer [render
                                      render-resource]]
            [com.stuartsierra.component :as component]
            [dealer.devsvcctrl :as dev-ctrl]
            [neowrap.neowrapper :as neo4j]))

;;;; https://xeranet.atlassian.net/wiki/display/NSN/2015/09/25/Harvesting+Schedules
;;;; https://xeranet.atlassian.net/wiki/display/SPS/Daylight+Harvesting+Functional+Spec

;;; Use mapdb or something later on
(defonce ambient-light-values
  ;;Keep a map of nodeid to ambient light sensor readings.
  (atom {}))

(defonce group-state
  ;;A map from profile-id to state information.
  (atom {}))

(def ^:private is-trigger-cypher
  "Given a nodeid, return whether it is a DHTrigger."
  "MATCH (n:Node {nodeid: \"{{nodeid}}\" })-[:DH {trigger: true}]-(:DHProfile) return n")

(defn is-trigger?
  "Return whether a given nodeid identifies a Daylight Harvesting Trigger node."
  [nodeid]
  (let [query (render is-trigger-cypher {:nodeid nodeid})
        mycount (count (json/read-str (neo4j/executeQuery query)))]
    (not (zero? mycount))))

(defonce ^:dynamic *dhprofileid*
  nil)

(defn iqm
  "Calculate IQM as per Scala code.

  TODO: Determine what to do when `samples` is empty."
  [samples]
  (spyf :info "IQM ambient light samples %s" (vec samples))
  (when (seq samples)
    (let [n (count samples)
          quarter-n (Math/floor (* n 0.25))
          samples (->> samples
                       sort
                       (drop quarter-n)
                       (drop-last quarter-n))
          sum (apply + samples)
          count (count samples)
          mean (/ sum count)]
      (debugf "DHProfileid: %s\nIQM: %s"
              *dhprofileid* mean)
      mean)))

(defn iterate
  "Calculate the sucessive driver output level and error level given a
  reading and previous output and error levels."
  [{:keys [gain
           reset-time
           min-drive
           max-drive
           set-point
           slew-rate]
    :as config}
   in out0 error0]
  ;; Assert we have configuration values.
  {:pre [config gain reset-time min-drive max-drive set-point slew-rate]}
  (let [;; Compute error as percent of setPoint
        error1 (/ (- set-point in)
                  set-point)
        ;; Calculate error velocity for PI equation; enforce maximum slew rate
        delta (Math/max (* slew-rate -100.0)
                        (Math/min (* gain
                                     (- (* 100.0
                                           (inc (/ reset-time))
                                           error1)
                                        error0))
                                  (* slew-rate 100.0)))
        ;; Slew control output between min and max drive levels
        out1 (Math/max min-drive
                       (Math/min (+ out0 delta)
                                 max-drive))]
    ;; log.debug(f"iterate: PV = $in%.3f; e = $error1%.3f; dCO = $delta%.3f; CO = $out%.3f")
    ;; publishSensorVars(out, in)
    (debugf "dhprofileid: %s\niterate IN out %s error %s OUT out %s error %s"
            *dhprofileid*
            (str out0) (str error0)
            (str out1) (str error1))
    [out1 error1 delta]))

(defn normalize-to-seq
  [value]
  (if-not (seq value)
    []
    (if (map? value)
      [value]
      value)))

(def get-nodeids
  #(mapv :nodeid %))

(defn get-dhprofile
  [dhprofileid]
  (let [{{:keys [beginTime
                 endTime]
          :as dhprofile} :dhprofile
         :keys [site]
         :as query-result} (-> "cyphers/get_dhprofile.cypher"
                               io/resource
                               slurp
                               (neo4j/executeQuery {"dhprofileid" dhprofileid})
                               (json/read-str :key-fn keyword))]
    (when-not (empty? query-result)
      (if-not (and beginTime
                   endTime)
        (do (warn "DHProfile lacks beginTime/endTime"
                  query-result)
            dhprofile)
        (-> query-result
            (set/rename-keys {:dhprofile :profile})
            light-utils/resolve-symbolic-time)))))

(defonce ^:private suspended-profiles
  (atom #{}))

(defonce calibration-values
  (atom {}))

(def calibration-timeout
  "Maximum amount of time to wait for all Daylight Harvesting Profile
  nodes to return ambient light sensor values."
  60e3)

(defn calibrate
  [dhprofileid]
  (let [{:keys [triggers
                controllers]}
        (-> "cyphers/get_dhprofile_nodes.cypher"
            io/resource
            slurp
            (neo4j/executeQuery {"dhprofileid" dhprofileid})
            (json/read-str :key-fn keyword)
            (update :controllers get-nodeids)
            (update :triggers (comp set
                                    get-nodeids)))
        {:keys [maxDrive]} (get-dhprofile dhprofileid)]
    ;; Herein, I use throw `ex-info` exceptions as a form of flow
    ;; control in a very Python-like fashion. I believe this would be
    ;; considered a code-smell by Java programmers.
    (try
      (debugf "Calibrating %s" dhprofileid)
      (when-not (seq controllers)
        (throw (ex-info "Cannot calibrate DHProfile with no Nodes."
                        {:dhprofileid dhprofileid})))
      (when-not (seq triggers)
        (throw (ex-info "No triggers present in DHProfile"
                        {:dhprofileid dhprofileid
                         :controllers controllers})))
      (when (@suspended-profiles dhprofileid)
        (throw (ex-info "Calibration already in progress"
                        {:dhprofileid dhprofileid})))
      (debugf "Adding %s to suspended dhprofiles" dhprofileid)
      (swap! suspended-profiles conj dhprofileid)
      (debugf "Initializing response map for %s's calibration samples" dhprofileid)
      (swap! calibration-values assoc dhprofileid {})
      (debugf "%s: Sending maximum LightingForceState to: %s" dhprofileid controllers)
      (dev-ctrl/query-exec-msgpk {:nodeprops {:type "LightingForceState"
                                              :nodeid controllers
                                              :level maxDrive
                                              :timeout 0
                                              :harvesting true}})
      (let [fetch-responses #(@calibration-values dhprofileid)
            fetch-all-responses (future
                                  ;; TODO: Why are we polling here?
                                  ;; Isn't one of the major points of
                                  ;; v3.0 that it's event driven?!
                                  (loop [responses nil]
                                    (if (= triggers (-> responses
                                                        keys
                                                        set))
                                      (do (debugf "Received responses for calibrating %s: %s"
                                                 dhprofileid
                                                 responses)
                                          responses)
                                      (do (Thread/sleep 10e3)
                                          (recur (fetch-responses))))))
            responses (or (deref fetch-all-responses
                                 calibration-timeout
                                 nil)
                          (throw (ex-info "Did not get a ambient light response from all trigger nodes."
                                          {:dhprofileid dhprofileid
                                           :triggers triggers
                                           :responses (vec (fetch-responses))})))
            sample (binding [*dhprofileid* dhprofileid]
                     (iqm (vals responses)))
            setPoint (* 1.02 sample)
            calibrated-values (stringify-keys {:setPoint setPoint})]
        (debugf "Updating %s with calibrated setPoint value %s" dhprofileid setPoint)
        (-> "cyphers/calibrate_dhprofile.cypher"
            io/resource
            slurp
            (neo4j/executeQuery {"dhprofileid" dhprofileid
                                 "setPoint" setPoint})))
      (catch clojure.lang.ExceptionInfo e
        (doto e
          warn))
      (finally
        (debugf "Cleaning up response map for %s" dhprofileid)
        (swap! calibration-values dissoc dhprofileid)
        (debugf "Removing %s from suspended dhprofiles" dhprofileid)
        (swap! suspended-profiles disj dhprofileid)))))

(defn find-dh-group
  [nodeid]
  (let [query-results (->> {:nodeid nodeid}
                           (render-resource "templates/find_dhprofile_and_nodes.cypher.tmpl")
                           neo4j/executeQuery
                           json/read-str
                           keywordize-keys)]
    (-> query-results
        (update :profile #(if (map? %)
                            %
                            (first %)))
        (update :controllers (comp get-nodeids
                                   normalize-to-seq))
        (update :triggers (comp get-nodeids
                                normalize-to-seq)))))

(defn record-ambient-light
  "Update `ambient-light-value`, calculate new IQM sample, and set driver output level.

  Also calculate error level for next iteration."
  [nodeid value]
  (if (is-trigger? nodeid)
    (let [value (/ value 1000) ;; Convert from millilux to lux
          {:keys [profile]} (find-dh-group nodeid)
          {:keys [dhprofileid]} profile
          suspended (@suspended-profiles dhprofileid)]
      (debugf "Received ambient light value %s from trigger node %s, associated with dhprofile %s"
             value
             nodeid
             dhprofileid)
      (if-not suspended
        (do (debugf "DHProfile not suspended: %s" dhprofileid)
            (swap! ambient-light-values assoc nodeid value))
        (do (debugf "DHProfile suspended for calibration: %s" dhprofileid)
            (swap! calibration-values assoc-in [dhprofileid nodeid] value))))
    (debugf "Node %s has no DHProfile" nodeid)))

(defonce fast-poll
  (atom #{}))

(defonce slow-poll
  (atom #{}))

(defonce inactive-profiles
  (atom #{}))

(def fast-poll-minutes
  1)

(def slow-poll-minutes
  10)

(def output-history-length
  (quot slow-poll-minutes
        fast-poll-minutes))

(defonce fast-poll-counter
  (atom (dec fast-poll-minutes)))

(defonce slow-poll-counter
  (atom (dec slow-poll-minutes)))

(defn get-dh-nodes
  [dhprofileid]
  (let [query-results (-> "cyphers/get_dhprofile_nodes.cypher"
                          io/resource
                          slurp
                          (neo4j/executeQuery {"dhprofileid" dhprofileid})
                          (json/read-str :key-fn keyword))]
    (-> query-results
        (update :controllers get-nodeids)
        (update :triggers get-nodeids))))

(defn light
  [nodeids level]
  (if-let [nodeids (seq nodeids)]
    (do
      (debugf "DHProfile %s sending LightingForceState command: driver level %s to %s" *dhprofileid* level nodeids)
      (dev-ctrl/query-exec-msgpk {:nodeprops {:type "LightingForceState"
                                              :nodeid nodeids
                                              :pri 4
                                              :level level
                                              :timeout 0}}))
    (debugf "Not sending LFS as there are no nodes to send to.")))

(def update-deltas
  (fnil (comp butlast
              conj)
        (repeat output-history-length 0)))

(defn run-controller
  [{:as dhprofile
    :keys [dhprofileid
           setPoint
           gain
           resetTime
           minDrive
           maxDrive
           slewRate]}]
  (let [suspended (@suspended-profiles dhprofileid)
        {:keys [controllers triggers]} (get-dh-nodes dhprofileid)]
    (cond
      (nil? setPoint) (debugf "Profile %s needs calibration" dhprofileid)
      suspended (debugf "Profiles %s is currently suspended for calibration" dhprofileid)
      (empty? controllers) (debugf "Profile %s has no controller nodes to drive" dhprofileid)
      (empty? triggers) (debugf "Profile %s has no trigger nodes to sample from" dhprofileid)
      :else (binding [*dhprofileid* dhprofileid]
              (let [sample (->> triggers
                                (map #(get @ambient-light-values % 0.0))
                                iqm)]
                (if (> sample
                       (* 1.5
                          setPoint))
                  (light controllers minDrive)
                  (let [{:keys [out error] :as state} (@group-state dhprofileid
                                                       {:error 0 :out 0})
                        config {:gain (double gain)
                                :reset-time (double resetTime)
                                :min-drive (double minDrive)
                                :max-drive (double maxDrive)
                                :set-point (double setPoint)
                                :slew-rate (double slewRate)}
                        [out error delta]  (iterate config sample out error)
                        out (int out)]
                    (swap! group-state assoc
                           dhprofileid (assoc state
                                              :out out
                                              :error error))
                    (swap! group-state update-in
                           [dhprofileid :deltas]
                           update-deltas
                           delta)
                    (light controllers out))))))))

(def fast-profiles-to-run
  (async/chan))

(def slow-profiles-to-run
  (async/chan))

(def fifteen-minute-counter
  "Set to minutes remaining til quarter of the hour."
  (atom (- 15
           (rem (time-core/minute (time-core/now))
                15))))

(defn recently-1am
  "Return the timezone names which have recently become midnight."
  [datetime]
  (let [minus-fifteen-min (time-core/minus datetime (time-core/minutes 15))
        within-last-fifteen-min? #(time-core/within? minus-fifteen-min
                                                     datetime
                                                     %)
        beginning-of-day (-> datetime
                             .withTimeAtStartOfDay
                             (time-core/plus (time-core/hours 1)))
        beginning-of-tomorrow (-> datetime
                                  (time-core/plus (time-core/days 1))
                                  .withTimeAtStartOfDay
                                  (time-core/plus (time-core/hours 1)))]
    (->> (time-core/available-ids)
         (filter (fn [time-zone]
                   (let [time-zone (time-core/time-zone-for-id time-zone)
                         [beginning-of-day
                          beginning-of-tomorrow] (map #(time-core/from-time-zone % time-zone)
                                                      [beginning-of-day
                                                       beginning-of-tomorrow])]
                     (or (within-last-fifteen-min? beginning-of-day)
                         (within-last-fifteen-min? beginning-of-tomorrow)))))
         vec)))

(defn dhprofiles-to-calibrate
  [datetime]
  (let [props (spy :debug (stringify-keys {:timezones (recently-1am datetime)}))]
    (-> "MATCH (s:Site)-[:HAS]->(p:DHProfile {autocalibrate: true, autocalibrateoptout: false})-[:HAS]->(n:Node)
WHERE s.time_zone in {props}.timezones
WITH {
sitename: s.name,
siteid: s.siteid,
dhprofileid: p.dhprofileid
} AS items
RETURN collect(distinct(items)) AS items"
        (neo4j/executeQuery props)
        (json/read-str :key-fn keyword)
        (->> (spy :debug))
        :items)))

(def autocalibration-mutex
  (atom 1))

(defn autocalibration
  [datetime]
  (debug "Calibrating DH Profiles for sites at 1am.")
  (try
    (when (zero? (swap! autocalibration-mutex dec))
      (->> (dhprofiles-to-calibrate datetime)
           (spy :debug)
           (map (fn [{:keys [siteid
                             dhprofileid]
                      :as sdhp}]
                  (debugf "Autocalibrating DHProfile: %s" sdhp)
                  ;; TODO: Introduce a jitter to not calibrate all DH
                  ;; Profiles at once, but in groups over a timespan.
                  (calibrate dhprofileid)
                  (-> "MATCH (p:DHProfile {dhprofileid: {props}.dhprofileid})
                       SET p.last_calibrated = {props}.now"
                      (neo4j/executeQuery {"dhprofileid" dhprofileid
                                           "now" (str datetime)}))))
           dorun))
    (catch Exception e
      (error e)))
  (swap! autocalibration-mutex inc))

(defn schedule-loop
  []
  ;; Grab profiles that need to move
  (let [get-dhprofile (memoize get-dhprofile)
        move-from-fast (remove (comp light-utils/in-schedule
                                     get-dhprofile)
                               @fast-poll)
        move-from-slow (remove (comp light-utils/in-schedule
                                     get-dhprofile)
                               @slow-poll)
        move-from-inactive (filter (comp light-utils/in-schedule
                                         get-dhprofile)
                                   @inactive-profiles)]
    ;; Now active profiles from inactive to `fast-poll`
    (light-utils/atom-set-move move-from-inactive
                               inactive-profiles
                               fast-poll)
    ;; Inactive `fast-poll` profiles to `inactive-profiles`
    (light-utils/atom-set-move move-from-fast
                               fast-poll
                               inactive-profiles)
    ;; Inactive `slow-poll` profiles to `inactive-profiles`
    (light-utils/atom-set-move move-from-slow
                               slow-poll
                               inactive-profiles)
    (->> (concat move-from-fast
                 move-from-slow)
         (map get-dhprofile)
         (mapcat :nodes)
         light-utils/reset-lights))
  ;(spy @inactive-profiles)
  (when (zero? (swap! fast-poll-counter (comp #(mod % fast-poll-minutes)
                                              dec)))
    (debug "Running Daylight Harvesting Scheduler fast-poll")
    (async/onto-chan fast-profiles-to-run
                     (spy :debug @fast-poll)
                     false))
  (when (zero? (spyf :debug
                     "Minutes until slow-poll processing: %s"
                     (swap! slow-poll-counter (comp #(mod % slow-poll-minutes)
                                                    dec))))
    (debug "Running Daylight Harvesting Scheduler slow-poll")
    (async/onto-chan slow-profiles-to-run
                     (spy :debug @slow-poll)
                     false))
  (when (zero? (swap! fifteen-minute-counter (comp #(mod % 15)
                                                   dec)))
    (debug "Calculating sites at 1AM with DHProfiles to calibrate")
    (try
      (autocalibration (time-core/now))
      (catch Exception e
        (error e)))))

(defn schedule-new-profile
  [dhprofileid]
  (swap! fast-poll conj dhprofileid))

(defn dh-scheduler
  []
  (when-let [dhprofileids (-> "MATCH (p:DHProfile) RETURN collect(distinct({dhprofileid: p.dhprofileid})) as items"
                              neo4j/executeQuery
                              (json/read-str :key-fn keyword)
                              :items
                              (->> (map :dhprofileid))
                              seq)]
    (apply swap! fast-poll conj dhprofileids))
  (try
    (info "Starting the daylight harvesting control loop.")
    (loop []
      (schedule-loop)
      (Thread/sleep 60e3)
      (recur))
    (catch java.lang.InterruptedException e
      (info "Daylight harvesting control loop is ending."))
    (catch Exception e
      (error e))))

(defrecord DHScheduler [dh-scheduler-thread]
  component/Lifecycle
  (start [component]
    (let [thread (Thread. dh-scheduler)
          sink-channel (async/chan (async/dropping-buffer 0))]
      (info "Starting daylight harvesting loop in a thread.")
      (.start thread)
      (async/pipeline-blocking 2
                               sink-channel
                               (map (fn [dhprofileid]
                                      (try
                                        (run-controller (get-dhprofile dhprofileid))
                                        (when-let [dhdeltas (get-in @group-state [dhprofileid :deltas])]
                                          (when (every? #(< % 2)
                                                        dhdeltas)
                                            (swap! fast-poll disj dhprofileid)
                                            (swap! slow-poll conj dhprofileid)))
                                        (catch Throwable e
                                          (error e)))
                                      true))
                               fast-profiles-to-run
                               false
                               #(error %))
      (async/pipeline-blocking 2
                               sink-channel
                               (map (fn [dhprofileid]
                                      (try
                                        (run-controller (get-dhprofile dhprofileid))
                                        (when-let [dhdeltas (get-in @group-state [dhprofileid :deltas])]
                                          (when (> (last dhdeltas) 2)
                                            (swap! slow-poll disj dhprofileid)
                                            (swap! fast-poll conj dhprofileid)))
                                        (catch Throwable e
                                          (error e)))
                                      true))
                               slow-profiles-to-run
                               false
                               #(error %))
      (assoc component
             :dh-scheduler-thread thread)))
  (stop [component]
    (info "Stopping DH schedule updater.")
    (when dh-scheduler-thread
      (.interrupt dh-scheduler-thread))
    (dissoc component :dh-scheduler-thread)))

(defn new-dh-scheduler
  []
  (map->DHScheduler {}))
