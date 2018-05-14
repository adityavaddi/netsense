(ns ^{:doc "Proximity Dimming Application."}
    appl.proximity-dimming
  (:gen-class)
  (:require [appl.lighting-control.utils :as light-utils]
            [clojure.set :as set]
            [clj-time
             [coerce :as coerce-time]
             [core :as clj-time]]
            [clojure.core.async :as async]
            [clojure.data.json :as json]
            [logging.activitylogger :as actlog]
            [clojure.java.io :as io]
            [clojure.tools.logging :refer :all]
            [com.stuartsierra.component :as component]
            [metrics.meters :as meters]
            [metrics.timers :as timers]
            [dealer metrics
             [devsvcctrl :as ctrl]]
            [neowrap.neowrapper :as neo4j]
            [utils
             [geocalc :as gcal]
             [solartime :as sol]]))

(defonce metric-factory
  (dealer.metrics/metric-factory-stub "appl.proximity-dimming"))

(defn node-time-to-joda
  [node-time]
  (coerce-time/from-long
   (long
    (quot node-time 1000))))

(defonce presence-values
  ;; Locally cache the `nodeid`, `time`, and `value` of presence
  ;; sensor samples coming in to avoid unnecessary burden on the DB
  ;; from excessive lookups.
  (atom nil))

(defn off-and-for-less-than
  "Provide a predicate that determines whether an entry of
  `presence-values` was 0 and before a given time `t`."
  [t]
  (fn [[nodeid {:keys [time value]}]]
    (and (zero? value)
         (or (clj-time/before? time t)
             (clj-time/equal? time t)))))

(defn recently-active-nodes
  [updated-motion-map profile-nodes t]
  (with-open [_ (timers/start (metric-factory :timer
                                              ["recently-active-nodes"]))]
    (->> (map :nodeid profile-nodes)
         (select-keys updated-motion-map)
         (remove (off-and-for-less-than t))
         (map first)
         (map (fn [nodeid]
                (first (filter (comp #{nodeid}
                                     :nodeid) profile-nodes)))))))

(defn parse-lat-lon
  "Take a node's latitude and longitude as strings and parse them as
  `Float`s. Log a `WARN` and return nil if not possible."
  [{:keys [lat lon]
    :as node}]
  (if-not (and lat lon)
    (warn (ex-info "Both latitude and longitude are not present on node"
                   node))
    (try
      (let [lat (Float/parseFloat lat)
            lon (Float/parseFloat lon)]
        (assoc node
               :lat lat
               :lon lon))
      (catch Exception e
        (warn e)
        (warn (ex-info "Unable to parse both latitude and longitude from node"
                       node))))))

(defn get-neighbors
  [nodes radius node]
  (with-open [_ (timers/start (metric-factory :timer
                                              ["get-neighbors"]))]
    (cond
      ;; If a pdprofile is assigned to a node directly
      (= [node] nodes) []
      ;; If we're operating in no-radius mode
      (nil? radius) nodes
      ;; If we're operating in "motion-detector" mode
      (zero? radius) []
      :else (if-let [node (spy :debug (parse-lat-lon node))]
              (let [nodes (map #(or (parse-lat-lon %)
                                    (assoc % :ignored true))
                               nodes)
                    ignored (filter :ignored nodes)
                    nodes (remove :ignored nodes)]
                (when (seq ignored)
                  (warn (ex-info "Ignoring potential neighbors lacking calculable lat/lon"
                                 {:ignored ignored})))
                (gcal/proximity node
                                radius
                                nodes))
              (do (warn (ex-info "Ignoring node's neighbors as it has no lat/lon"
                                 node))
                  [])))))

(defn send-dimming-command [{:as nodeprops
                             :keys [nodeid
                                    level]}]
  (meters/mark! (metric-factory :meter
                                ["LFS"]))
  (debugf "Sending LightingForceState from proximity dimming to nodes %s at level %s"
         nodeid
         level)
  (let [msg {:nodeprops nodeprops}]
    (ctrl/query-exec-msgpk msg)))

(defonce dim-values
  (atom {}))

(defn dim-values-update-node
  "Helper function for `handle-lt-value`'s `swap!`."
  [{current-time :time
    :as current-value}
   {potential-time :time
    :as potential-value}]
  (if (or (nil? current-time)
          (> potential-time current-time))
    potential-value
    current-value))

(defn handle-lt-value
  [{:keys [nodeid
           value
           time]
    :as sensor-sample}]
  {:pre [nodeid
         value
         time]}
  (let [values (-> sensor-sample
                   (select-keys [:value
                                 :time])
                   (update :value int))]
    (swap! dim-values update nodeid
           dim-values-update-node
           values)))

(defn light
  "Send a LFS to `nodeids` for `level`, but only if they're not
  already at that driver level. We check our local cache, which is
  populated by `lt`, C*, and failing those, what we're about to send."
  [nodeids level]
  (with-open [_ (timers/start (metric-factory :timer
                                              ["light"]))]
    (debugf "maybe send LFS (lvl=%s) for nodes %s" level nodeids)
    (if (seq nodeids)
      (let [level (int level)
            ;; Before taking a snapshot of the cache for paring down our
            ;; `nodeids` candidates, let's take a snapshot to see what
            ;; cache misses we're likely to find, and then backfill
            ;; appropriately.
            cache @dim-values
            cache-fails (filter (fn [nodeid]
                                  (nil? (get cache nodeid)))
                                nodeids)
            cassandra-backfill (map (juxt identity
                                          actlog/get_latest_light_mode)
                                    cache-fails)
            cassandra-misses (->> cassandra-backfill
                                  (filter (fn [[nodeid {:keys [driver]}]]
                                            (nil? driver)))
                                  (map (fn [[nodeid _]]
                                         nodeid)))]
        (doseq [[nodeid
                 {:keys [driver
                         time]
                  ;; TODO: Update Cassandra tables to hold
                  ;; timestamp. Remove hack below.
                  :or {time 0}}]
                cassandra-backfill]
          (when (some? driver)
            (handle-lt-value {:nodeid nodeid
                              :value driver
                              :time time})))
        (let [ ;; Remove nodes which `dim-values` say are already at the
              ;; correct light driver level. If they're not in this
              ;; table, then we assume they're not at the right level.
              cache @dim-values
              filtered-nodeids (remove (fn [nodeid]
                                         (= (:value (get cache nodeid))
                                            level))
                                       nodeids)]
          (if (seq filtered-nodeids)
            (do
              ;; Record our intent to send a LFS to these Nodes. It is
              ;; intended that it will be overwritten by an `lt` sensor
              ;; sample.
              (let [now (-> (light-utils/get-now)
                            coerce-time/to-long
                            (* 1000))]
                (doseq [nodeid nodeids]
                  (handle-lt-value {:nodeid nodeid
                                    :value level
                                    :time now})))
              (send-dimming-command {:type "LightingForceState"
                                     :nodeid (vec filtered-nodeids)
                                     :level level
                                     :timeout 0
                                     :pri 4}))
            (debugf "Not sending LFS (lvl=%s) as there are no nodes to send to after checking last level" level))))
      (debugf "Not sending LFS (lvl=%s) as there are no nodes to send to." level))))

(defn do-lighting
  "Partition all lights in a PDProfile to dim and to undim."
  [updated-motion-map
   profile-nodes
   {dim :minLevel
    undim :maxLevel
    :keys [pdprofileid
           neighbor-lookup
           radius detection_duration]}
   time]
  (with-open [_ (timers/start (metric-factory :timer
                                              ["do-lighting"]))]
   (let [watermark (clj-time/minus time (clj-time/seconds detection_duration))
         primary-active-nodes (->> (recently-active-nodes updated-motion-map profile-nodes watermark)
                                   (map :nodeid))
         secondarily-active-nodes (mapcat #(get neighbor-lookup %)
                                          primary-active-nodes)
         active-nodes (->> (concat primary-active-nodes
                                   secondarily-active-nodes)
                           set)
         inactive-nodes (->> (map :nodeid profile-nodes)
                             (remove active-nodes)
                             set)]

     (debugf "Doing Lighting for %s ... active %s inactive %s" pdprofileid active-nodes inactive-nodes)
     [(light inactive-nodes dim)
      (light active-nodes undim)])))

(defonce active-pdprofiles
  ;; "A list of currently active Proximity Dimming profile IDs."
  (atom #{}))

(defonce inactive-pdprofiles
  ;; "A list of currently inactive Proximity Dimming profile IDs."
  (atom #{}))

(defonce cache-pd-nodes (atom {}))

(defn invalidate-cache-pd-nodes [pdprofileid]
  (swap! cache-pd-nodes dissoc pdprofileid))

(defn add-cache-pd-nodes [pdprofileid nodes]
  (swap! cache-pd-nodes assoc pdprofileid nodes))

(defn get-pd-nodes
  [pdprofileid]
  (with-open [_ (timers/start (metric-factory :timer
                                              ["profile-node-fetch"]))]
    (let [cached (get @cache-pd-nodes pdprofileid)]
      (if cached
        cached
        (let [query-results (-> "cyphers/find_pdprofile_nodes.cypher"
                                io/resource
                                slurp
                                (neo4j/executeQuery {"pdprofileid" pdprofileid})
                                (json/read-str :key-fn keyword))
              nodes (update query-results
                            :nodes #(if (map? %)
                                      [%]
                                      %))]
          (add-cache-pd-nodes pdprofileid nodes)
          nodes)))))

(defn run-controller
  "Given a PDProfile, calculate what light levels should be set for
  nodes assigned to it."
  [{:keys [pdprofileid]
    :as pdprofile}]
  (with-open [_ (timers/start (metric-factory :timer
                                              ["run-controller"]))]
    (if-not (light-utils/in-schedule pdprofile)
      (debugf "Profile %s not active at this time." pdprofileid)
      (let [{:keys [nodes]} (get-pd-nodes pdprofileid)
            joda-time (light-utils/get-now)]
        (if-not (seq nodes)
          (debugf "Not running PD controller for %s as there are no nodes associated with it" pdprofileid)
          (do-lighting @presence-values
                       nodes
                       pdprofile
                       joda-time))))))

(defonce pdprofile-cache
  (atom {}))

(defn invalidate-pdprofile-cache-for-id
  [pdprofileid]
  (invalidate-cache-pd-nodes pdprofileid)
  (swap! pdprofile-cache dissoc pdprofileid))

(defn update-radius
  [{{:keys [mode]
     :as pdprofile} :pdprofile
    :as cypher-return}]
  (if (= "no-radius"
         mode)
    (update-in cypher-return
               [:pdprofile]
               dissoc :radius)
    cypher-return))

(defn add-neighbors
  [{{:keys [pdprofileid
            radius
            mode]
     :as pdprofile} :pdprofile
    :as cypher-return}]
  (let [{:keys [nodes]} (get-pd-nodes pdprofileid)]
    (assoc-in cypher-return
              [:pdprofile
               :neighbor-lookup]
              (->> nodes
                   (map (juxt :nodeid
                              #(->> (get-neighbors nodes
                                                   (if (= "no-radius" mode) nil radius)
                                                   %)
                                    (map :nodeid)
                                    set)))
                   (into {})))))

(defn get-pdprofile
  "Grab the PDProfile from Neo4j and, when applicable, adjust the time
  expressions with respect to the site's lat/lon."
  [pdprofileid]
  (with-open [_ (timers/start (metric-factory :timer
                                              ["profile-fetch"]))]
    (if-let [cached (get @pdprofile-cache pdprofileid)]
      (light-utils/resolve-symbolic-time cached)
      (let [cypher-return (-> "cyphers/get_pdprofile.cypher"
                              io/resource
                              slurp
                              (neo4j/executeQuery {"pdprofileid" pdprofileid})
                              (json/read-str :key-fn keyword))]
        (when-not (empty? cypher-return)
          (let [pdprofile (-> cypher-return
                              ;update-radius
                              add-neighbors
                              (set/rename-keys {:pdprofile :profile}))]
            (swap! pdprofile-cache assoc pdprofileid pdprofile)
            (light-utils/resolve-symbolic-time pdprofile)))))))

(defn presence-values-update-node
  "Helper function for `handle-presence-value`'s `swap!`. We want to
  store the most recent `1.0`, but not update a `0.0` if already
  present."
  [{current-time :time
    current-value :value
    :as current-sample}
   {potential-time :time
    potential-value :value
    :as potential-sample}]
  (cond
    ;; If we've already got a `0.0`, discard fresher `0.0` valued
    ;; samples.
    (and (= 0.0 current-value potential-value)
         potential-time
         current-time
         (clj-time/after? potential-time current-time)) current-sample
    ;; Otherwise, we're optimistically accepting any new potential
    ;; sample, assuming we're receiving things in monotonically
    ;; increasing time.
    :else potential-sample))

(defonce cache-profile-from-node (atom {}))

(defn invalidate-cache-profile-from-node [nodeid]
  (let [pdprofileid (get @cache-profile-from-node nodeid)]
    (if pdprofileid (invalidate-cache-pd-nodes pdprofileid))
    (swap! cache-profile-from-node dissoc nodeid)))

(defn invalidate-cache-profile-from-nodes [nodeids]
  (doall (map invalidate-cache-profile-from-node nodeids)))

(defn get-profile-from-node
  [nodeid]
  (with-open [_ (timers/start (metric-factory :timer
                                              ["profile-from-node-timer"]))]
    (let [cached (get @cache-profile-from-node nodeid)]
      (if cached
        cached
        (let [profile (-> "cyphers/find_pdprofile_from_node.cypher"
                          io/resource
                          slurp
                          (neo4j/executeQuery {"nodeid" nodeid})
                          (json/read-str :key-fn keyword)
                          :pdprofileid)]
          (swap! cache-profile-from-node assoc nodeid profile)
          profile
          )))))

(defn handle-presence-value
  "Handle a sensor sample value and update the watermark, when
  appropriate."
  [{:keys [time nodeid value]
    :as sensor-sample}]
  {:pre [time nodeid value]}
  (with-open [_ (timers/start (metric-factory :timer
                                              ["handle-presence-value"]))]
    (let [joda-time (node-time-to-joda time)
          pdprofileid (get-profile-from-node nodeid)
          new-light-values (swap! presence-values update nodeid
                                  presence-values-update-node
                                  {:value value
                                   :time joda-time})]
      (case value
        1.0 (with-open [_ (timers/start (metric-factory :timer
                                                        ["immediate-run-controller"]))]
              (debugf "Received motion presence detected from node %s at time %s" nodeid joda-time)
              (when pdprofileid
                (-> pdprofileid
                    get-pdprofile
                    run-controller)))
        0.0 (debugf "Received motion presence = 0 from node %s at time %s" nodeid joda-time)
        (warn (ex-info "Received malformed presence."
                       (assoc sensor-sample
                              :joda-time joda-time))))
      new-light-values)))

(defonce active-profiles-to-run
  (async/chan))

(defn schedule-loop
  []
  (try
    (debug "Running Proximity Dimming Scheduler...")
    (let [move-to-active (filter (comp light-utils/in-schedule
                                       get-pdprofile)
                                 @inactive-pdprofiles)
          move-to-inactive (remove (comp light-utils/in-schedule
                                         get-pdprofile)
                                   @active-pdprofiles)]
      (light-utils/atom-set-move move-to-active
                                 inactive-pdprofiles
                                 active-pdprofiles)
      (light-utils/atom-set-move move-to-inactive
                                 active-pdprofiles
                                 inactive-pdprofiles)
      (->> move-to-inactive
           (map get-pdprofile)
           (mapcat :nodes)
           (map :nodeid)
           light-utils/reset-lights))
    ;(spy @inactive-pdprofiles)
    (async/onto-chan active-profiles-to-run  @active-pdprofiles false)
    (catch Throwable t
      (error t))))

(defn schedule-new-profile
  [pdprofileid]
  (swap! active-pdprofiles conj pdprofileid))

(defn pd-scheduler
  [polling-interval]
  (when-let [pdprofileids (-> "MATCH (:Site)-[:HAS]->(p:PDProfile) RETURN collect(distinct({pdprofileid: p.pdprofileid})) as items"
                              neo4j/executeQuery
                              (json/read-str :key-fn keyword)
                              :items
                              (->> (map :pdprofileid))
                              seq)]
    (apply swap! active-pdprofiles conj pdprofileids))
  (try
    (debug "Starting the proximity dimming control loop.")
    (loop []
      (schedule-loop)
      (Thread/sleep polling-interval)
      (recur))
    (catch java.lang.InterruptedException e
      (debug "Proximity dimming control loop is ending."))
    (catch Exception e
      (error e))))

(defn handle-cape
  [cape-operation jsmap]
  {:pre [(map? jsmap)]}
  (case cape-operation
    ("applyPDtoNodes"
     "deletePDProfile") (invalidate-pdprofile-cache-for-id (get-in jsmap [:pdprofileprops
                                                                          :pdprofileid]))
    "updatePDProfile" (let [{:keys [pdprofileid]} (:pdprofileprops jsmap)]
                        (invalidate-pdprofile-cache-for-id pdprofileid)
                        (-> pdprofileid
                            get-pdprofile
                            run-controller))
    "createPDProfile" (let [{:keys [pdprofileid]} (:pdprofileprops jsmap)]
                        (schedule-new-profile pdprofileid)
                        (invalidate-pdprofile-cache-for-id pdprofileid))
    "updateNode" (when-let [pdprofileid (get-profile-from-node (get-in jsmap [:nodeprops :nodeid]))]
                   (invalidate-pdprofile-cache-for-id pdprofileid))
    (warn (ex-info "Dropping message from CAPE"
                   jsmap))))

(defrecord PDScheduler [polling-interval
                        pd-scheduler-thread
                        metrics-registry]
  component/Lifecycle
  (start [component]
    (let [{:keys [registry
                  prefix]} metrics-registry
          polling-interval (* 1000
                              (or (when polling-interval
                                    (try
                                      (Integer/parseInt polling-interval)
                                      (catch Exception e
                                        (warnf "Unable to parse polling-interval value as integer: %s" polling-interval)
                                        (warn e))))
                                  30))
          thread (Thread. (partial pd-scheduler
                                   polling-interval))
          sink-channel (async/chan (async/dropping-buffer 0))]
      (alter-var-root #'metric-factory
                      (fn [& _]
                        (dealer.metrics/metric-factory-factory "appl.proximity-dimming"
                                                               registry
                                                               prefix)))
      (info "Starting proximity dimming loop in a thread.")
      (.start thread)
      ;; Consume "active" pdprofiles
      (async/pipeline-blocking 3
                               sink-channel
                               (map (fn [pdprofileid]
                                      (try
                                        (-> pdprofileid
                                            get-pdprofile
                                            run-controller)
                                        (catch Throwable e
                                          (error e)))
                                      true))
                               active-profiles-to-run
                               false
                               #(error %))
      (assoc component
             :pd-scheduler-thread thread)))
  (stop [component]
    (info "Stopping PD schedule updater.")
    (when pd-scheduler-thread
      (.interrupt pd-scheduler-thread))
    (dissoc component :pd-scheduler-thread)))

(defn new-pd-scheduler
  [polling-interval]
  (map->PDScheduler {:polling-interval polling-interval}))
