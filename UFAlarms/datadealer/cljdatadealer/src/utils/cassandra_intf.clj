(ns ^{:doc "The Cassandra Interface."}
    utils.cassandra_intf
  (:gen-class)
  (:refer-clojure :exclude [update])
  (:require [clj-time
             [coerce :as co]
             [core :as clj-time]
             [format :as format]]
            [clojure.data.json :as json]
            [dealer.metrics]
            [clojure.walk :refer :all]
            [qbits.hayt :refer :all]
            [utils.cassandra-cql :as cql]
            [com.stuartsierra.component :as component]
            [clojure.tools.logging :refer :all]
            [metrics.timers :as timers]
            [utils.config :as conf]))

(def cassandra-timer-name "cassandra-timer")
;(defonce cassandra-timer (timers/timer cassandra-timer-name))

(defonce metric-factory
  (dealer.metrics/metric-factory-stub "utils.cassandra_intf"))

(def options (conf/cassservice))

(def conn (delay
           (if-let [connected (try (cql/establish-connection)
                                   (catch Exception e
                                     (error e)
                                     (spyf :error "Cassandra connection failed: %s" e)
                                     false))]
             connected
             (do
               (Thread/sleep 5000)
               (recur)))))

(def key_space (:keyspace options))
(def otalog_tab "ota_logs")
(def otaidslog_tab "ota_ids_logs")
(def actlog_tab "activity_logs_user")
(def actlog_tab_site "activity_logs_site")
(def actlog_tab_login "activity_logs_login")
(def conn_status "connection_status")
(def sensor_samples "device_sensor_samples")
(def parking_status "parking_spot_current")
(def parking_zone_status "parking_zone_status")
(def parking_history "parking_spot_historic")
(def parking_aggregate_site "aggregation_parking_site")
(def parking_aggregate_spot "aggregation_parking_spot")
(def parking_aggregate_zone "aggregation_parking_zone")
(def parking_aggregate_group "aggregation_parking_group")
(def alarms "device_alarms")
(def lighting "light")
(def node_status "node_status")
(def traffic_point "traffic_point")
(def traffic_detection_way "traffic_detection_way")
(def traffic_detection_area "traffic_detection_area")
(def traffic_status "traffic_status")
(def traffic_current_status "traffic_current_status")
(def traffic_config "traffic_config")
(def traffic_history "traffic_history")
(def traffic_aggregation_events_node "aggregation_traffic_events")
(def traffic_aggregation_events_site "aggregation_traffic_events_site")
(def aggregate_energy_node "aggregation_energy_savings_node")
(def aggregate_energy_site "aggregation_energy_savings_site")
(def orghierarchy_by_nodeid_tab "orghierarchy_by_nodeid")

(defn exception-cause
  [e]
  (let [cause (.getCause e)]
    (spy :error cause)
    (if (nil? cause)
      "unknown"
      (.toString cause))))

(defn store_node_hierarchy [data]
  (with-open [_ (timers/start (metric-factory :timer
                                              [orghierarchy_by_nodeid_tab "insert"]))]
    (cql/use-keyspace @conn key_space)
    ;(debugf "Storing node hierarchy %s into table %s" data orghierarchy_by_nodeid_tab)
    (cql/insert-async @conn orghierarchy_by_nodeid_tab data)))

(defn get_node_hierarchy
  [nodeid]
  {:pre [nodeid]}
  (with-open [_ (timers/start (metric-factory :timer
                                              [orghierarchy_by_nodeid_tab "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn orghierarchy_by_nodeid_tab
                  (columns :orgid :siteid)
                  (where [[= :nodeid nodeid]])
                  (limit 1))))

(defn store_ota_log [log_map]
  (with-open [_ (timers/start (metric-factory :timer
                                              [otalog_tab "insert"]))]
      (cql/use-keyspace @conn key_space)
      (let [insertvalues (merge {:when (now)} log_map)]
        ;(debugf "Storing ota log %s into table %s" log_map otalog_tab)
        (cql/insert-async @conn otalog_tab insertvalues))))

(defn store_otaids_log [log_map]
  (with-open [_ (timers/start (metric-factory :timer
                                              [otaidslog_tab "insert"]))]
      (cql/use-keyspace @conn key_space)
      (let [insertvalues (merge {:when (now)} log_map)]
        ;(debugf "Storing ota log %s into table %s" log_map otaidslog_tab)
        (cql/insert-async @conn otaidslog_tab insertvalues))))

(defn org_site_from_ota_log
  [jobid]
  {:pre [jobid]}
  (with-open [_ (timers/start (metric-factory :timer
                                              [otaidslog_tab "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn otaidslog_tab
                  (columns :orgid :siteid :target_type :target_id :model :node_count)
                  (where [[= :jobid jobid]])
                  (limit 1))))

(defn ota_log_by_site
  [orgid siteid]
  {:pre [orgid siteid]}
  (with-open [_ (timers/start (metric-factory :timer
                                              [otalog_tab "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn otalog_tab
                  (columns (-> (unix-timestamp-of :when) (as "when"))
                           :jobid
                           :firmwareid
                           :description
                           :target_type
                           :target_id
                           :orgid
                           :siteid
                           :model
                           :node_count)
                  (where [[= :orgid orgid]
                          [= :siteid siteid]]))))

(defn ota_log_by_job
  [orgid siteid, jobid]
  {:pre [orgid siteid]}
  (with-open [_ (timers/start (metric-factory :timer
                                              [otalog_tab "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn otalog_tab
                  (columns (-> (unix-timestamp-of :when) (as "when"))
                           :jobid
                           :firmwareid
                           :nodeid
                           :status
                           :success
                           :description
                           :target_type
                           :target_id
                           :orgid
                           :siteid
                           :model
                           :node_count)
                  (where [[= :orgid orgid]
                          [= :siteid siteid]
                          [= :jobid jobid]]))))

(defn act_logs [userid siteid daterange]
  (let [minDate (.toDate (format/parse (:datemin daterange)))
        maxDate (.toDate (format/parse (:datemax daterange)))
        maxtimeuuid (max-timeuuid minDate)
        mintimeuuid (min-timeuuid maxDate)]
    (with-open [_ (timers/start (metric-factory :timer
                                                [actlog_tab_site "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn actlog_tab_site
                  (columns  (-> (unix-timestamp-of :when) (as "when")) :siteid :userid :targettype :targetid :activity :message)
                  (where [[= :siteid siteid]
                          [> :when maxtimeuuid]
                          [< :when mintimeuuid]])
                  (limit 10000)))))

(defn get_act_logs [userid siteid daterange]
  (try
    (let [results (act_logs userid siteid daterange)
      res (if (empty? results) [] results)]
      res)
    (catch Exception e
      (error e)
      (throw
        (ex-info (format "Exception in cassandra query %s" actlog_tab)
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defn store_activity_log [actlog_table log_map]
  (with-open [_ (timers/start (metric-factory :timer
                                              [actlog_table "insert"]))]
      (cql/use-keyspace @conn key_space)
      (let [insertvalues (merge {:when (now)} log_map {:message (json/write-str (:message log_map))})]
        ;(debugf "Storing activity log %s into table %s" log_map actlog_table)
        (cql/insert-async @conn actlog_table insertvalues))))

(defn get-when
  [{:keys [when] :or {when nil} :as log_map}]
  (if when
    (assoc log_map :when
                    (-> when
                        (format/parse)
                        (.toDate)
                        (min-timeuuid)))
    log_map))

(defn log_activity
  [type log_map]
  (case type
    "user" (try
             (->> log_map
              (get-when)
               (store_activity_log actlog_tab))
             (catch Exception e
               (error e)
               (throw
                 (ex-info (format "Exception inserting into table %s" actlog_tab)
                          {:message  (.getMessage e) :cause (exception-cause e)}))))

    "site" (try
             (->> log_map
                  (get-when)
                  (store_activity_log actlog_tab_site))
             (catch Exception e
               (error e)
               (throw
                 (ex-info (format "Exception inserting into table %s" actlog_tab_site)
                          {:message  (.getMessage e) :cause (exception-cause e)}))))

    "login" (try
              (->> log_map
                   (get-when)
                   (store_activity_log actlog_tab_login))
             (catch Exception e
               (error e)
               (throw
                 (ex-info (format "Exception inserting into table %s" actlog_tab_login)
                          {:message  (.getMessage e) :cause (exception-cause e)}))))))


(defn last_seen
  [email]
  {:pre [email]}
  (with-open [_ (timers/start (metric-factory :timer
                                              [actlog_tab_login "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn actlog_tab_login
                  (columns  :when)
                  (where [[= :targetid email] [= :targettype "email"] [= :activity "succeeded"]])
                  (order-by [:targettype :desc] [:activity :desc] [:when :desc])
                  (limit 1))))

(defn attempts_since [email tuuid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [actlog_tab_login "select count"]))]
      (cql/use-keyspace @conn key_space)
      (cql/perform-count @conn actlog_tab_login
                    (if-not (nil? tuuid)
                        (where [[= :targetid email] [= :targettype "email"] [= :activity "failed"] [> :when tuuid]])
                        (where [[= :targetid email] [= :targettype "email"] [= :activity "failed"]])))))

(declare get_latest_connection_status)

(defn store_log_connection_status [nodeid isconnected]
  (with-open [_ (timers/start (metric-factory :timer
                                              [conn_status "insert"]))]
      (cql/use-keyspace @conn key_space)
      ;(debugf "Inserting into %s nodeid %s isconnected %s" conn_status nodeid isconnected)
      (let [insertvalues {
                          :nodeid nodeid
                          :isconnected isconnected
                          :since (now)
                          }]
        (cql/insert @conn conn_status insertvalues))))

(defn store_light_mode [log_map]
  (cql/use-keyspace @conn key_space)
  ;FIXME: INSERT statement in a LOOP
  (doseq [nodeid (:nodeid log_map)]
    (let [log_map_ex (assoc log_map :nodeid nodeid)]
      (with-open [_ (timers/start (metric-factory :timer
                                                  [lighting "insert"]))]
        (cql/insert-async @conn lighting log_map_ex)))))


(defn parkingspot_latest_object
  [parkingspotid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [parking_status "select"]))]
      (cql/use-keyspace @conn key_space)
      (:objectid (first (cql/select @conn parking_status
                  (columns :parkingspotid :objectid)
                  (where [[= :parkingspotid parkingspotid]]))))))

(defn perform_store_parking_historic [log_map]
  (with-open [_ (timers/start (metric-factory :timer
                                              [parking_history "insert"]))]
    (cql/use-keyspace @conn key_space)
    (cql/insert-async @conn parking_history log_map)))

(defn store_parking_historic [log_map]
    ;(spy :debug (format "Inserting into %s %s" parking_history log_map))
    (let [time-millis (quot (long (:since log_map)) 1000)
           insertvalues {
        :nodeid           (:nodeid log_map)
        :channel          (:channel log_map)
        :siteid           (:siteid log_map)
        :orgid            (:orgid log_map)
        :parkinggroupid   (:parkinggroupid log_map)
        :parkingzoneid    (:parkingzoneid log_map)
        :parkingspotid    (:parkingspotid log_map)
        :occupancy        (:occupancy log_map)
        :objectid         (:objectid log_map)
        :since            (:rsince log_map)
        :date             (format/unparse (format/formatters :date) (co/from-long time-millis))
      }
      iv (into {}(remove #(nil? (val %)) insertvalues))
      previous (parkingspot_latest_object (:parkingspotid log_map))
      iv2 (merge iv {:objectid previous :occupancy false :since (- (:since iv) 50)})
      iv1 (if (= (:occupancy log_map) false) (merge iv {:objectid previous}) iv)]
      (if (and (:occupancy log_map) (not= (:objectid log_map) previous)) (perform_store_parking_historic iv2))
      (perform_store_parking_historic iv1)))

(defn perform_store_parking_current [log_map]
  (with-open [_ (timers/start (metric-factory :timer
                                              [parking_status "insert"]))]
    (cql/use-keyspace @conn key_space)
    (cql/insert-async @conn parking_status log_map)))

(defn store_parking_current_status [log_map]
    (if (some? (:since log_map)) (store_parking_historic log_map))
    (perform_store_parking_current (into {}(remove #(nil? (val %)) (merge log_map {:rsince nil})))))

(defn store_parking_zone [log_map]
  (with-open [_ (timers/start (metric-factory :timer
                                              [parking_zone_status "insert"]))]
    (cql/use-keyspace @conn key_space)
    (cql/insert @conn parking_zone_status log_map)))

(defn get_current_zone_status [siteid parkingzoneid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [parking_zone_status "select"]))]
      (cql/use-keyspace @conn key_space)
      (first (cql/select @conn parking_zone_status
                                                   (columns :config)
                                                   (where [[= :siteid siteid] [= :parkingzoneid parkingzoneid]])
                                                   (limit 1)))))

(defn get_node_parking_zone_configs [nodeid siteid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [parking_zone_status "select"]))]
    (cql/use-keyspace @conn key_space)
    (map #(keywordize-keys (json/read-str (:config %)))
         (cql/select @conn parking_zone_status
                     (columns :config)
                     (where [[= :siteid siteid]])))))

(defn get_node_active_parking_zone_configs [nodeid siteid]
  (filter #(and (:active %) (= nodeid (:nodeid %))) (get_node_parking_zone_configs nodeid siteid)))

(defn delete_spot [spot]
  (with-open [_ (timers/start (metric-factory :timer
                                              [parking_status "delete"]))]
      (cql/use-keyspace @conn key_space)
      (cql/delete-async @conn parking_status spot)))

(defn get_current_spots [parkingzoneid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [parking_status "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn parking_status
                         (columns :parkingspotid :active)
                         (where [[= :parkingzoneid parkingzoneid]]))))

(defn store_parking_spot_removed [psid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [parking_status "delete"]))]
    (cql/use-keyspace @conn key_space)
    ;(spy :debug (format "Deleting from %s %s" parking_status psid))
    (cql/delete-async @conn parking_status (where [[= :parkingspotid psid]]))))

(defn store_parking_zone_removed [siteid parkingzoneid]
      (let [current (get_current_zone_status siteid parkingzoneid)
            cfg (keywordize-keys (json/read-str (:config current)))
            spots  (get_current_spots parkingzoneid)
            updated-cfg (json/write-str (assoc cfg :active false ))]
        ;(spy :debug (format "Disabling from %s %s %s" parking_zone_status parkingzoneid updated-cfg))
        (store_parking_zone {:config updated-cfg :parkingzoneid parkingzoneid :siteid siteid})
        ; Delete spots
        (doseq [spot spots]
          (store_parking_spot_removed (:parkingspotid spot)))
        ))

(defn get_parking_spots_status [pzid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [parking_status "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn parking_status
                                 (columns :parkingspotid)
                                 (where [[= :parkingzoneid pzid]]))))

(defn cleanup_parking_zone [pzid]
      (let [spots (get_parking_spots_status pzid)]
        (doseq [spot spots]
          (store_parking_spot_removed (:parkingspotid spot))
        )
      ))

(defn store_traffic_current_status [log_map]
  (cql/use-keyspace @conn key_space)
  ;(spy :debug (format "Inserting into %s %s" traffic_status log_map))
  (with-open [_ (timers/start (metric-factory :timer
                                              [traffic_current_status "insert"]))]
    (cql/insert-async @conn traffic_current_status log_map)))
    ; NSN-12428 DWH reads from Kafka now
    ;(with-open [_ (timers/start (metric-factory :timer
    ;                                          [traffic_status "insert"]))]
    ;  (cql/insert-async @conn traffic_status (merge log_map {:date formatted-date}))))

(defn data->config [data]
  (keywordize-keys (json/read-str data)))

(defn get_node_traffic_config [nodeid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [traffic_config "select"]))]
    (cql/use-keyspace @conn key_space)
    (map #(update-in % [:data] data->config)
         (cql/select @conn traffic_config
                (columns :eventid :active :data)
                (where [[= :nodeid nodeid]])))))

(defn get_node_active_traffic_config [nodeid]
  (filter #(:active (:data %)) (get_node_traffic_config nodeid)))

(defn store_traffic_config [log_map]
  (cql/use-keyspace @conn key_space)
  ;(spy :debug (format "Inserting into %s %s" traffic_config log_map))
  (with-open [_ (timers/start (metric-factory :timer
                                              [traffic_config "insert"]))]
  (cql/insert-async @conn traffic_config log_map)))

(defn store_traffic_config_removed [eventid]
  (cql/use-keyspace @conn key_space)
  (let [current   (with-open [_ (timers/start (metric-factory :timer
                                                              [traffic_config "select"]))]
                    (first (cql/select @conn traffic_config
                            (columns :data)
                            (where [[= :eventid eventid]]))))
        cfg (keywordize-keys (json/read-str (:data current)))
        updated-cfg (json/write-str (assoc cfg :active false ))]
    ;(spy :debug (format "Disabling in %s %s %s" traffic_config eventid updated-cfg))
    (with-open [_ (timers/start (metric-factory :timer
                                                [traffic_config "insert"]))]
    (cql/insert-async @conn traffic_config {:active false :eventid eventid :data updated-cfg}))
    ))

(defn store_traffic_current_removed [eventid]
  (cql/use-keyspace @conn key_space)
  ;(spy :debug (format "Deleting from %s %s" traffic_current_status eventid))
  (with-open [_ (timers/start (metric-factory :timer
                                              [traffic_current_status "delete"]))]
    (cql/delete-async @conn traffic_current_status (where [[= :trafficdetectioneventid eventid]]))))

(defn store_sensor_sample [log_map]
  (with-open [_ (timers/start (metric-factory :timer
                                              [sensor_samples "insert"]))]
      (cql/use-keyspace @conn key_space)
      ;debugf "Inserting into %s %s" sensor_samples log_map)
      (let [insertvalues (merge log_map {:date (now)})]
        (if (= (:sensor log_map) "lt") (store_light_mode {:nodeid [(:nodeid log_map)] :driver (int (:value log_map))}))
        (cql/insert-async @conn sensor_samples insertvalues))))

(defn store_alarm_activity [log_map]
  (with-open [_ (timers/start (metric-factory :timer
                                              [alarms "insert"]))]
      (cql/use-keyspace @conn key_space)
      ;(debugf "Inserting into %s %s" alarms log_map)
      (let [insertvalues (merge log_map {:date (now)})]
        (cql/insert-async @conn alarms insertvalues))))

(defn log_parking_historic [log_map]
  (try
    (store_parking_historic log_map)
    (catch Exception e
      (error e)
      (spy :error
        (ex-info (format "Exception inserting into table %s" parking_history)
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defn log_parking_current_status [log_map]
  (try
    (store_parking_current_status log_map)
    (catch Exception e
      (error e)
      (spy :error
        (ex-info (format "Exception inserting into table %s parkingspotid %s" parking_status (:parkingspotid log_map))
                 {:message  (.getMessage e)
                  :cause   (exception-cause e)})))))

(defn log_parking_zone [log_map]
  (try
    (store_parking_zone log_map)
    (catch Exception e
      (error e)
      (spy :error
        (ex-info (format "Exception inserting into table %s" parking_zone_status)
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defn log_parking_zone_removed [siteid eid]
  (try
    (store_parking_zone_removed siteid eid)
    (catch Exception e
      (error e)
      (spy :error
       (ex-info (format "Exception deleting from table %s" parking_zone_status)
                {:message  (.getMessage e)
                 :cause    (exception-cause e)})))))

(defn log_parking_spot_removed [psid]
  (try
    (store_parking_spot_removed psid)
    (catch Exception e
      (error e)
      (spy :error
        (ex-info (format "Exception deleting from table %s" parking_status)
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defn log_traffic_current_status [log_map]
  (try
    (let [
    ;;insertvalues log_map]
           insertvalues (merge log_map {:updated (now)})]

      (store_traffic_current_status insertvalues))

    (catch Exception e
           (error e)
           (spy :error
             (ex-info (format "Exception inserting into table %s" traffic_current_status)
                      {:message  (.getMessage e)
                       :cause    (exception-cause e)})))))

(defn log_traffic_config [log_map]
  (try
    (let [ insertvalues (merge log_map {:updated (now)})]
      (store_traffic_config insertvalues))

    (catch Exception e
           (error e)
           (spy :error
             (ex-info (format "Exception inserting into table %s" traffic_config)
                      {:message  (.getMessage e)
                       :cause    (exception-cause e)})))))

(defn log_traffic_config_removed [eid]
  (try
    (store_traffic_config_removed eid)
    (catch Exception e
           (error e)
           (spy :error
             (ex-info (format "Exception deleting from table %s" traffic_config)
                      {:message  (.getMessage e)
                       :cause    (exception-cause e)})))))

(defn log_traffic_current_removed [eid]
  (try
    (store_traffic_current_removed eid)
    (catch Exception e
           (error e)
           (spy :error
             (ex-info (format "Exception deleting from table %s" traffic_current_status)
                      {:message  (.getMessage e)
                       :cause    (exception-cause e)})))))

(defn log_connection_status [nodeid isconnected]
  (let [currentstate (get_latest_connection_status nodeid)]
    (if (not= (:isconnected currentstate) isconnected)
      (try
        (store_log_connection_status nodeid isconnected)
      (catch Exception e
        (error e)
        (throw
          (ex-info (format "Exception inserting into table %s" conn_status)
                   {:message  (.getMessage e)
                    :cause    (exception-cause e)})))))))

(defn log_light_mode [log_map]
  (try
    (store_light_mode log_map)
    (catch Exception e
      (error e)
      (throw
        (ex-info (format "Exception inserting into table %s" lighting)
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defn log_sensor_sample [log_map]
  (try
    (store_sensor_sample log_map)
    (catch Exception e
      (error e)
      (throw
        (ex-info (format "Exception inserting into table %s" sensor_samples)
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defn get_latest_connection_count
  [nodeid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [sensor_samples "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn sensor_samples
                  (columns :value)
                  (where [[= :nodeid nodeid] [= :sensor "cc"]])
                  (order-by [:sensor :desc] [:time :desc])
                  (limit 1))))

(defn log_increment_connect_count [nodeid]
  (let [count (get_latest_connection_count nodeid)
        cnt (if (empty? count) 0 (:value (first count)))
        dt (clj-time/now)
        ldt (unchecked-multiply (co/to-long dt) 1000)
        ]
    (try
      (log_sensor_sample {:nodeid nodeid :sensor "cc" :value (+ cnt 1) :time ldt :date dt})
      (catch Exception e
        (spyf :error "Cassandra exception %s" e)
        (throw
          (ex-info (format "Exception incrementing connection counter for node %s" nodeid)
                   {:message (.getMessage e)
                    :cause   (exception-cause e)}))))))

(defn log_device_alarms [log_map]
  (try
    (store_alarm_activity log_map)
    (catch Exception e
      (error e)
      (throw
        (ex-info (format "Exception inserting into table %s" alarms)
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defn latest_light_mode
  [nodeid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [lighting "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn lighting
                              (columns :nodeid
                                       :harvest_trigger
                                       :policy
                                       :driver
                                       :priority
                                       :isscheduled
                                       :startdt)
                              (where [[= :nodeid nodeid]])
                              (limit 1))))

(defn latest_sensor_samples
  [nodeid sensor date maxresults]
  (with-open [_ (timers/start (metric-factory :timer
                                              [sensor_samples "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn sensor_samples
                  (columns :time :value)
                  (where [[= :nodeid nodeid]
                          [= :sensor sensor]
                          [> :time (co/to-long date)]])
                  (order-by [:sensor :desc] [:time :desc])
                  (limit maxresults))))

(defn latest_energy_samples_node
  [nodeid date1 date2 maxresults period siteid]
  (let [from  (* (.getTime date1) 1000)
        to    (* (.getTime date2) 1000)]
    (with-open [_ (timers/start (metric-factory :timer
                                                [aggregate_energy_node "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn aggregate_energy_node
                  (columns :siteid :nodeid :startdt :legacy_energy_consumption :led_energy_consumption :actual_energy_consumption :savings_legacy_vs_led :savings_legacy_vs_actual)
                  (where [[= :siteid siteid]
                          [= :nodeid nodeid]
                          [= :aggregation_type period]
                          [> :starttime from]
                          [< :starttime to]])
                  (order-by [:starttime])
                  (limit maxresults)))))

(defn latest_energy_samples_site
  [date1 date2 maxresults period siteid]
  (let [from  (* (.getTime date1) 1000)
        to    (* (.getTime date2) 1000)]
    (with-open [_ (timers/start (metric-factory :timer
                                                [aggregate_energy_site "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn aggregate_energy_site
                  (columns :siteid :startdt :legacy_energy_consumption :led_energy_consumption :actual_energy_consumption :savings_legacy_vs_led :savings_legacy_vs_actual)
                  (where [[= :siteid siteid]
                          [= :aggregation_type period]
                          [> :starttime from]
                          [< :starttime to]])
                  (order-by [:starttime])
                  (limit maxresults)))))

(defn latest_energy_samples
  [nodeid date1 date2 limit period siteid]
  (let [results (if (= nodeid "all") (latest_energy_samples_site date1 date2 limit period siteid) (latest_energy_samples_node nodeid date1 date2 limit period siteid))]
    results))

 (defn latest_sensor_samples_ft
   [nodeid sensor date1 date2 maxresults]
  (let [from  (* (.getTime date1) 1000)
        to    (* (.getTime date2) 1000)]
    (with-open [_ (timers/start (metric-factory :timer
                                                [sensor_samples "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn sensor_samples
                  (columns :time :value)
                  (where [[= :nodeid nodeid] [= :sensor sensor] [>= :time from] [<= :time to]])
                  (order-by [:sensor :desc] [:time :desc])
                  (limit maxresults)))))

(defn latest_connection_status
  [nodeid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [conn_status "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn conn_status
                  (columns :since :isconnected)
                  (where [[= :nodeid nodeid]])
                  (order-by [:since :desc])
                  (limit 1))))

(defn transform-connection-status [m]
  (dissoc (assoc m :isconnected (= (:net_stat m) 1)) :net_stat))

(defn latest_connection_status
  [nodeid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [conn_status "select"]))]
    (cql/use-keyspace @conn key_space)
    (map transform-connection-status
         (cql/select @conn node_status
                (columns :since :net_stat)
                (where [[= :nodeid nodeid]])
                (limit 1)))))

(defn get_parkingzone  [siteid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [parking_zone_status "select"]))]
  (cql/use-keyspace @conn key_space)
  (cql/select @conn parking_zone_status
              (columns :orgid :siteid :parkinggroupid :parkingzoneid :type :config :state)
              (where [[= :siteid siteid]]))))

(defn get_one_parkingzone  [siteid parkingzoneid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [parking_zone_status "select"]))]
  (cql/use-keyspace @conn key_space)
  (cql/select @conn parking_zone_status
              (columns :orgid :siteid :parkinggroupid :parkingzoneid :type :config :state)
              (where [[= :siteid siteid] [= :parkingzoneid parkingzoneid]]))))

(defn latest_parkingzone_status
  [parkingzoneid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [parking_status "select"]))]
  (cql/use-keyspace @conn key_space)
  (cql/select @conn parking_status
              (columns :siteid :parkinggroupid :parkingzoneid :parkingspotid :x1 :x2 :y1 :y2 :co :lat1 :lat2 :lng1 :lng2 :address :description :altitude :since :tags :occupancy :objectid :active :activesince)
              (where [[= :parkingzoneid parkingzoneid]])
              (order-by [:parkingspotid :desc]))))

(defn latest_parkingspot_status
  [parkingspotid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [parking_status "select"]))]
  (cql/use-keyspace @conn key_space)
  (cql/select @conn parking_status
              (columns :siteid :parkinggroupid :parkingzoneid :parkingspotid :x1 :x2 :y1 :y2 :lat1 :lat2 :lng1 :lng2 :address :description :altitude :since :tags :occupancy :objectid :active :activesince)
              (where [[= :parkingspotid parkingspotid]]))))

(defn latest_parkinggroup_status
  [parkinggroupid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [parking_status "select"]))]
  (cql/use-keyspace @conn key_space)
  (cql/select @conn parking_status
              (columns :siteid :parkinggroupid :parkingzoneid :parkingspotid :x1 :x2 :y1 :y2 :lat1 :lat2 :lng1 :lng2 :address :description :altitude :since :tags :occupancy :objectid :active :activesince)
              (where [[= :parkinggroupid parkinggroupid]])
              (order-by [:parkingzoneid :parkingspotid]))))

(defn latest_parkingsite_status
  [siteid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [parking_status "select"]))]
  (cql/use-keyspace @conn key_space)
  (cql/select @conn parking_status
              (columns :orgid :siteid :parkinggroupid :parkingzoneid :parkingspotid :nodeid :channel :demarcated :x1 :x2 :y1 :y2 :lat1 :lat2 :lat3 :lat4 :lng1 :lng2 :lng3 :lng4 :address :description :altitude :since :tags :occupancy :objectid :active :activesince)
              (where [[= :siteid siteid]]))))

(defn parkingsite_history
  [siteid filter]
  (let [from (:from filter)
        to (:to filter)
        in (:spatialResolution filter)
        spotid (:parkingspotid filter)
        zoneid (:parkingzoneid filter)
        groupid (:parkinggroupid filter)
        type (:type filter)]
  (cql/use-keyspace @conn key_space)
  (case in
    "site"
    (with-open [_ (timers/start (metric-factory :timer
                                                [parking_aggregate_site "select"]))]
    (cql/select @conn parking_aggregate_site
                (columns :startdt :enddt :siteid :parkingtype :occpercent :occduration :turnovers)
                (where [[= :siteid siteid] [= :parkingtype type] [> :starttime from] [< :starttime to]])))
    "group"
    (with-open [_ (timers/start (metric-factory :timer
                                                [parking_aggregate_group "select"]))]
      (cql/select @conn parking_aggregate_group
                (columns :startdt :enddt :siteid :parkingtype :groupid :occpercent :occduration :turnovers)
                (where [[= :siteid siteid] [= :groupid groupid] [= :parkingtype type] [> :starttime from] [< :starttime to]])))
    "zone"
    (with-open [_ (timers/start (metric-factory :timer
                                                           [parking_aggregate_zone "select"]))]
      (cql/select @conn parking_aggregate_zone
                  (columns :startdt :enddt :siteid :parkingtype :zoneid :occpercent :occduration :turnovers)
                  (where [[= :siteid siteid] [= :zoneid zoneid] [> :starttime from] [< :starttime to]])))
    "spot"
    ;(with-open [_ (timers/start (metric-factory :timer
    ;                                            [parking_aggregate_spot "select"]))]
    ;  (cql/select @conn parking_aggregate_spot
    ;            (columns :startdt :enddt :siteid :parkingtype :parkingspotid :occpercent :occduration :turnovers)
    ;            (where [[= :siteid siteid] [= :parkingtype "Demarcated"] [= :parkingspotid spotid] [> :starttime from] [< :starttime to]])))
      {"exception" "Aggregate Parking Spot data is no longer available"})))

; Currently not used in Netsense 3.0.4
;(defn latest_traffic_status
;  [siteid nodeid name]
;  (let [mwhere (concat [[= :siteid siteid]]
;                      (when nodeid [[= :nodeid nodeid]])
;                      (when name [[= :name name]])
;                      )]
;    (with-open [_ (timers/start (metric-factory :timer
;                                                [traffic_status "select"]))]
;      (cql/use-keyspace @conn key_space)
;      (cql/select @conn traffic_status
;                  (columns :data)
;                  (where mwhere)
;                  (limit 10000) (allow-filtering)))))

(defn current_traffic_status
  [siteid nodeid type]
  (let [mwhere (concat [[= :siteid siteid]]
                      (when nodeid [[= :nodeid nodeid]])
                      (when type [[= :type type]])
                      )]
    (with-open [_ (timers/start (metric-factory :timer
                                                [traffic_current_status "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn traffic_current_status
                  (columns :data :aggregated_count)
                  (where mwhere)
                  (limit 10000) (allow-filtering)))))

(defn traffic_history
  [siteid filter]
  (let [from (:from filter)
        to (:to filter)
        eventid (:eventid filter)
        nodeid (:nodeid filter)
        level (if nodeid "node" "site" )
        aggregation_type (:aggregation_type filter)
        type (:type filter)
        name (:name filter)
        mwhere (concat [[= :siteid siteid]]
                            (when nodeid [[= :nodeid nodeid]])
                            ;(when (and name (not= name 'all')) [[= :eventname name]])
                            (when (and eventid (not= level 'site')) [[= :eventid eventid]])
                            (when to [[<= :starttime to]])
                            (when from [[>= :starttime from]])
                            (when aggregation_type [[= :aggregation_type aggregation_type]])
                            )]
    (cql/use-keyspace @conn key_space)
    (case level
          "node"
      (with-open [_ (timers/start (metric-factory :timer
                                                  [traffic_aggregation_events_node "select"]))]
      (cql/select @conn traffic_aggregation_events_node
                             (case type
                                   "ObjectDwellEvent" (columns :startdt :enddt :siteid :nodeid :eventid :type (as :name :name) (as :objectclass :object_class) :eventcnt :avgdwell)
                               ("LineCrossingEvent" "ObjectLeavingEvent" "ObjectEnteringEvent") (columns :startdt :enddt :siteid :nodeid :eventid :type (as :name :name) (as :objectclass :object_class) :eventcnt :avgvelocity :medianvelocity :p85velocity)
                               (columns :startdt :enddt :siteid :nodeid :eventid :type (as :name :name) (as :objectclass :object_class) :eventcnt :avgvelocity :medianvelocity :p85velocity :avgdwell))
                             (where mwhere)
                             (limit 10000) (allow-filtering)))
          "site"
      (with-open [_ (timers/start (metric-factory :timer
                                                  [traffic_aggregation_events_site "select"]))]
        (cql/select @conn traffic_aggregation_events_site
                             (case type
                                   "ObjectDwellEvent" (columns :startdt :enddt :siteid :type (as :objectclass :object_class) :eventcnt :avgdwell)
                               ("LineCrossingEvent" "ObjectLeavingEvent" "ObjectEnteringEvent") (columns :startdt :enddt :siteid :type (as :objectclass :object_class) :eventcnt :avgvelocity :medianvelocity :p85velocity)
                               (columns :startdt :enddt :siteid :type (as :objectclass :object_class) :eventcnt :avgvelocity :medianvelocity :p85velocity :avgdwell))
                             (where mwhere)
                             (limit 10000) (allow-filtering)))
          (throw (ex-info (format "Level not supported %s" in)
                          {:message  (format "Level not supported %s" in)
                           :cause    :invalid-parameter
                           :status 400}))
          )))

(defn traffic_configs
  [siteid nodeid filter]
  (let [
         eventid (:eventid filter)
         ;;type (:type filter)
         type (:type filter)

         mwhere (concat [[= :siteid siteid]]
                       (when nodeid [[= :nodeid nodeid]])
                       (when (and type (not= type 'all')) [[= :type type]])
                       (when eventid [[= :eventid eventid]])
                       )]
    (with-open [_ (timers/start (metric-factory :timer
                                                [traffic_config "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn traffic_config
                  (columns :data)
                  (where mwhere)
                  (limit 10000) (allow-filtering)))))

(defn latest_site_nodes_statuses
  [siteid]
  (with-open [_ (timers/start (metric-factory :timer
                                              [node_status "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn node_status
                  (columns :nodeid :lig_stat :net_stat :sen_stat :siteid :orgid)
                  (where [[= :siteid siteid]])
                  (limit 10000))))

(defn latest_lf_nodes_statuses
  []
  (with-open [_ (timers/start (metric-factory :timer
                                              [node_status "select"]))]
      (cql/use-keyspace @conn key_space)
      (cql/select @conn node_status
                  (columns :nodeid :lig_stat :net_stat :sen_stat :siteid :orgid)
                  (where [[= :siteid "Unknown"]])
                  (limit 10000))))

(defn get_latest_light_mode
  [nodeid]
  {:pre [nodeid]}
  (try
    (let [result (latest_light_mode nodeid)]
      (if (empty? result) {} (first result)))
    (catch Exception e
      (error (.getMessage e))
      (throw
        (ex-info (format "Exception in cassandra query %s" lighting)
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defn get_node_status
  [nodeid]
  (try
    (let [result (latest_connection_status nodeid)]
      (if (empty? result) {} (first result)))
    (catch Exception e
      (error (.getMessage e))
      (throw
        (ex-info (format "Exception in cassandra query %s" node_status)
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defn get_parkingzone_status
  [parkingzoneid]
  (try
    (let [result (latest_parkingzone_status parkingzoneid)]
      (if (empty? result) {} (result)))
    (catch Exception e
      (error (.getMessage e))
      (spy :error
        (ex-info (format "Exception in cassandra query %s" parking_status)
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defn get_parkingsite_status
  [siteid]
  (try
    (let [result (latest_parkingsite_status siteid)]
      (if (empty? result) {} result))
    (catch Exception e
      (error (.getMessage e))
      (spy :error
        (ex-info (format "Exception in cassandra query %s" parking_status)
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defn get_parkingsite_history
  [siteid filter]
  (try
    (let [result (parkingsite_history siteid filter)]
      (if (empty? result) {} result))
    (catch Exception e
      (error (.getMessage e))
      (spy :error
        (ex-info (format "Exception in cassandra query %s" parking_history)
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defn get_parkinggroup_status
  [parkinggroupid]
  (try
    (let [result (latest_parkinggroup_status parkinggroupid)]
      (if (empty? result) {} (result)))
    (catch Exception e
      (error (.getMessage e))
      (spy :error
        (ex-info (format "Exception in cassandra query %s" parking_status)
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defn get_parkingspot_status
  [parkingspotid]
  (try
    (let [result (latest_parkingspot_status parkingspotid)]
      (if (empty? result) {} (result)))
    (catch Exception e
      (error (.getMessage e))
      (spy :error
        (ex-info (format "Exception in cassandra query %s" parking_status)
           {:message  (.getMessage e)
            :cause    (exception-cause e)})))))

(defn get_traffic_status
  [siteid nodeid type]
  (try
    (let [result (current_traffic_status siteid nodeid type)
          ;result_vec (vec (keep :data result))
          ;unpacked (map json/read-str result_vec)
          unpacked (map #(assoc (json/read-str (:data %)) :aggregated_count (:aggregated_count %)) result)
          ]
      (if (empty? result) {} unpacked))
    (catch Exception e
           (error (.getMessage e))
           (throw
             (ex-info (format "Exception in cassandra query %s" get_traffic_status)
                      {:message  (.getMessage e)
                       :cause    (exception-cause e)})))))

(defn get_traffic_history
  [siteid filter]
  (try
    (let [result (traffic_history siteid filter)]
      (if (empty? result) {} result))
    (catch Exception e
           (error (.getMessage e))
           (throw
             (ex-info (format "Exception in cassandra query %s" traffic_history)
                      {:message  (.getMessage e)
                       :cause    (exception-cause e)})))))

(defn get_traffic_config
  [siteid nodeid filter]
  (try
    (let [result (traffic_configs siteid nodeid filter)
          unpacked (map json/read-str (vec (keep :data result)))]
      (if (empty? result) {} unpacked))
    (catch Exception e
           (error (.getMessage e))
           (throw
             (ex-info (format "Exception in cassandra query %s" traffic_configs)
                      {:message  (.getMessage e)
                       :cause    (exception-cause e)})))))

(defn store_node_status [log_map]
  (with-open [_ (timers/start (metric-factory :timer
                                              [node_status "insert"]))]
  (cql/use-keyspace @conn key_space)
  (debugf "Inserting into %s %s" node_status log_map)
  (cql/insert-async @conn node_status (merge log_map {:since (now)}))))

(defn log_node_status [log_map]
  (try
    (store_node_status log_map)
    (catch Exception e
      (error e)
      (throw
        (ex-info (format "Exception inserting into table %s" node_status)
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defn compare-time-reverse [lhs rhs]
  (compare rhs lhs))

(defn format-micro-with-tz [microsec timezone]
  (-> microsec
      (quot 1000)
      co/from-long
      (clj-time/to-time-zone (clj-time/time-zone-for-id timezone))
      str))

(defn get_latest_sensor_samples
  [nodeid sensor date limit tz]
  (try
    (let [results (latest_sensor_samples nodeid sensor date limit)
          sorted-values (map #(clojure.core/update % :time (fn [d] (format-micro-with-tz d tz))) results)
          datevalues (into [] (sort-by :time compare-time-reverse sorted-values))]
      datevalues)
    (catch Exception e
      (error e)
      (throw
        (ex-info (format "Exception in cassandra query %s" sensor_samples)
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

 (defn get_latest_energy_ft
  [nodeid date1 date2 limit period siteid]
  (try
    (let [results (latest_energy_samples nodeid date1 date2 limit period siteid)]
      results)
    (catch Exception e
      (spyf :error "Cassandra exception %s" e)
      (throw
        (ex-info (format "Exception in cassandra query %s" aggregate_energy_node)
                                   {:message  (.getMessage e)
                                    :cause    (exception-cause e)})))))

 (defn get_latest_sensor_samples_ft
   [nodeid sensor date1 date2 limit period tz siteid]
   (if (= sensor "energy") (get_latest_energy_ft nodeid date1 date2 limit period siteid)
     (try
       (let [results (latest_sensor_samples_ft nodeid sensor date1 date2 limit)
             sorted-values (map #(clojure.core/update % :time (fn [d] (format-micro-with-tz d tz))) results)
             datevalues (into [] (sort-by :time compare-time-reverse sorted-values))]
         datevalues)
       (catch Exception e
         (error e)
         (throw
           (ex-info (format "Exception in cassandra query %s" sensor_samples)
                                      {:message  (.getMessage e)
                                       :cause    (exception-cause e)}))))))

(defn datetime-from-timestamp [t]
  (try
    (str (co/from-long (cql/unix-timestamp t)))
    (catch Exception e
      "")))

(defn get_latest_connection_status
  [nodeid]
  (try
    (let [results (latest_connection_status nodeid)
          res (if (empty? results) {:isconnected false :since ""} (clojure.core/update (first results) :since #(datetime-from-timestamp %)))]
      res)
    (catch Exception e
      (error e)
      (throw
        (ex-info (format "Exception in cassandra query %s" conn_status)
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defn get_latest_site_nodes_statuses
  [siteid]
  (try
    (let [results (latest_site_nodes_statuses siteid)
      fixed_net_stat (map #(update-in % [:net_stat] (fn [d] (if (= 1 d) true false))), results)
      res (if (empty? results) []  fixed_net_stat)]
      res)
    (catch Exception e
      (error e)
      (throw
        (ex-info (format "Exception in cassandra query %s: %s" latest_site_nodes_statuses (.getMessage e))
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defn get_latest_lf_nodes_statuses
  []
  (try
    (let [results (latest_lf_nodes_statuses )
      fixed_net_stat (map #(update-in % [:net_stat] (fn [d] (if (= 1 d) true false))), results)
      res (if (empty? results) []  fixed_net_stat)]
      res)
    (catch Exception e
      (error e)
      (throw
        (ex-info (format "Exception in cassandra query %s: %s" latest_lf_nodes_statuses (.getMessage e))
                 {:message  (.getMessage e)
                  :cause    (exception-cause e)})))))

(defrecord Cassandra [metrics-registry]
  component/Lifecycle
  (start [component]
    (let [{:keys [registry
                  prefix]} metrics-registry]
      (alter-var-root #'metric-factory
                      (fn [& _]
                        (dealer.metrics/metric-factory-factory "utils.cassandra_intf"
                                                               registry
                                                               prefix)))))
  (stop [component]
    component))

(defn new-cassandra
  []
  (map->Cassandra {}))
