(ns ^{:doc "The Activity Logger."
      :author "Chiradip Mandal"}
  logging.activitylogger (:gen-class)
  (:require [clojure.tools.logging :as logging])
  (:require [neowrap.neowrapper :as neo4j])
  (:require [clojure.data.json :as json])
  (:require [utils.cassandra_intf :as cass]
            [utils.cassandra-cql :as cql]
            [acl.core :as acl])
  (:use clostache.parser)
  (:use clojure.set))


;; A small utility method to get date field working
(defn json-date-writer [key value]
  (if (= key :when)
    (str (org.joda.time.DateTime. value))
    value))

(defn getactivitylogs [userid orgid siteid daterange]
  (logging/spy (str "[getactivitylogs] userid " userid " orgid " orgid " siteid " siteid " daterange " daterange))
  (cass/get_act_logs userid siteid daterange))

(defn log_node_hierarchy [data]
  (cass/store_node_hierarchy data)
  "Logged OK")

(defn get_node_hierarchy [nodeid]
  (cass/get_node_hierarchy nodeid))

(defn log_ota [log_map]
  (cass/store_ota_log log_map)
  "Logged OK")

(defn log_otaids [log_map]
  (cass/store_otaids_log log_map)
  "Logged OK")

(defn get_org_site_from_ota_log [jobid]
  (cass/org_site_from_ota_log jobid))

(defn get_ota_log_for_site [orgid siteid]
  (cass/ota_log_by_site orgid siteid))

(defn get_ota_log_for_job [orgid siteid jobid]
  (cass/ota_log_by_job orgid siteid jobid))

(defn log [activity log_map]
  (cass/log_activity activity log_map)
  "Logged OK")

(defn log_connection_status [nodeid isconnected]
  (cass/log_connection_status nodeid isconnected)
  "Logged OK")

(defn log_increment_connect_count [nodeid]
  (cass/log_increment_connect_count nodeid)
  "Logged OK")

(defn log_node_status [log_map]
  (cass/log_node_status log_map)
  "Logged OK")

(defn log_parking_historic [log_map]
  (cass/log_parking_historic log_map)
  "Logged OK")

(defn log_parking_current_status [log_map]
  (cass/log_parking_current_status log_map)
  "Logged OK")

(defn log_parking_zone [log_map]
  (cass/log_parking_zone log_map)
  "Logged OK")

(defn log_parking_zone_removed [siteid pzid]
  (cass/log_parking_zone_removed siteid pzid)
  "Logged OK")

(defn log_parking_spot_removed [psid]
  (cass/log_parking_spot_removed psid)
  "Logged OK")

(defn cleanup_parking_zone [pzid]
  (cass/cleanup_parking_zone pzid)
  "Logged OK")

(defn log_traffic_current_status [log_map]
  (cass/log_traffic_current_status log_map)
  "Logged OK")

(defn log_traffic_config [log_map]
  (cass/log_traffic_config log_map)
  "Logged OK")

(defn log_traffic_config_removed [psid]
  (cass/log_traffic_config_removed psid)
  "Logged OK")

(defn log_traffic_current_removed [psid]
  (cass/log_traffic_current_removed psid)
  "Logged OK")

(defn log_sensor_sample [log_map]
  (cass/log_sensor_sample log_map)
  "Logged OK")

(defn log_device_alarms [log_map]
  (cass/log_device_alarms log_map)
  "Logged OK")

(defn get_latest_parking_info
  [siteid]
  (cass/get_parkingsite_status siteid))

(defn get_parking_history
  [siteid filter]
  (cass/get_parkingsite_history siteid filter))

(defn get_latest_traffic_info
  [siteid nodeid type]
  (cass/get_traffic_status siteid nodeid type))

(defn get_traffic_history
  [siteid filter]
  (cass/get_traffic_history siteid filter))

(defn get_traffic_config
  [siteid nodeid filter]
  (cass/get_traffic_config siteid nodeid filter))

(defn get_latest_sensor_samples
  [nodeid sensor date limit tz]
  (cass/get_latest_sensor_samples nodeid sensor date limit tz))

(defn get_latest_connection_status
  [nodeid]
  (cass/get_latest_connection_status nodeid))

(defn get_latest_site_nodes_statuses
  [siteid]
  (cass/get_latest_site_nodes_statuses siteid))

(defn get_latest_lf_nodes_statuses
  []
  (cass/get_latest_lf_nodes_statuses))

(defn get_latest_sensor_samples_ft
  [nodeid sensor date1 date2 limit period tz siteid]
  (cass/get_latest_sensor_samples_ft nodeid sensor date1 date2 limit period tz siteid))

(defn get_node_active_parking_zone_configs
  [nodeid siteid]
  (cass/get_node_active_parking_zone_configs nodeid siteid))

(defn get_node_active_traffic_config
  [nodeid]
  (cass/get_node_active_traffic_config nodeid))

(defn get_current_spots
  [pkzid]
  (cass/get_current_spots pkzid))

(defn log_light_mode
  [{:keys [nodeid
           isscheduled
           policy
           harvest_trigger]
    :as log_map}]
  {:pre ([nodeid
          (some? isscheduled)
          policy
          (some? harvest_trigger)])}
  (cass/log_light_mode log_map))

(defn get_parkingzone
  [log_map]
  (cass/get_parkingzone log_map))

(defn get_one_parkingzone
  [siteid parkingzoneid]
  (cass/get_one_parkingzone siteid parkingzoneid))

(defn get_latest_light_mode
  [nodeid]
  (cass/get_latest_light_mode nodeid))
(defn get_access_stat
  [email]
  (let[
    result (logging/spy ( cass/last_seen email ) )
    ls_uuid (logging/spy (:when ( first result ) ) )
    last_seen_str (logging/spy (if-not (nil? ls_uuid) (str (java.sql.Date. (cql/unix-timestamp ls_uuid))) nil))
    attempts_since (logging/spy (cass/attempts_since email ls_uuid) )
    ] (logging/spy {:last_seen last_seen_str :login_attempts attempts_since} )
  ))
