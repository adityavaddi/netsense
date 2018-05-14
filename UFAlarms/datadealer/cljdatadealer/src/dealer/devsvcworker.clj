(ns ^{:doc "The Device Service Worker."}
    dealer.devsvcworker
  (:gen-class)
  (:require [amqp.core :as amqp]
            [clojure.stacktrace :as trace]
            [clj-uuid :as uuid]
            [clj-time
             [coerce :as c]
             [core :as time-core]
             [format :as f]]
            [clojure.data :as cdata]
            [clojure.set :as cset]
            [clojure.core.async :as async]
            [clojure.core.async.impl.concurrent :as conc]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.tools.logging :refer :all]
            [clojure.walk :refer :all]
            [clostache.parser :refer :all]
            [com.stuartsierra.component :as component]
            [dealer
             [devsvcctrl :as dev-ctrl]
             [worker :as worker]]
            [langohr
             [basic :as lb]
             [channel :as lch]
             [consumers :as lc]
             [core :as rmq]
             [exchange :as le]
             [queue :as lq]]
            [logging.activitylogger :as actlogger]
            [metrics
             [counters :as counters]
             [gauges :as gauge]
             [histograms :as histograms]
             [meters :as meters]
             [timers :as timers]]
            [mqtt.core :refer :all]
            [msgpack.core :as msgpk]
            [neowrap.neowrapper :as neo4j]
            [kafka.producer :as kafka]
            [kafka.consumer :as kafka-consumer]
            [utils
             [cape :as cape]
             [config :as conf]
             [schedules :as schedules]]
            [utils.async.async_chans :as ac]
            [clojure.string :as str])
  (:import java.util.concurrent.Executors
           com.verizon.netsense.utils.Z85))


(defonce metric-factory
  (dealer.metrics/metric-factory-stub "dealer.devsvcworker"))

(def devsvcworker-timer-name "device-service-worker")

(def zmqconf (conf/zmqservice))
(def mqtt-topic "DeviceWorker")

(defonce device-thread-pool-size (get (conf/ddconfig) :device-thread-pool-size 40))

(def thread-pool
  (Executors/newFixedThreadPool device-thread-pool-size
                                (conc/counted-thread-factory "device-worker-%d"
                                                             true)))

(defmacro myanotime
  "Evaluates expr and prints the time it took.  Returns the value of
 expr."
  [expr name]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     (debugf "[%s] Elapsed time: %s msecs" ~name (/ (double (- (. System (nanoTime)) start#)) 1000000.0))
     ret#))


(defn bytes? [x]
  (if (nil? x)
      false
      (= (Class/forName "[B")
         (.getClass x))))

(defn resolve-node [raw_msg]
  (with-open [_ (timers/start (metric-factory :timer
                                              ["resolve-node"]))]
    (let [cypher (-> "cyphers/get_site_and_org_for_node.cypher"
                     io/resource
                     slurp)
          results (keywordize-keys (get (json/read-str (neo4j/executeQuery cypher {"nodeid" (:nodeid raw_msg)})) "data"))]
      results)))

(defn resolve-video-node [raw_msg]
  (with-open [_ (timers/start (metric-factory :timer
                                              ["resolve-video-node"]))]
    (let [cypher (render-resource "templates/get_data_for_node.cypher.tmpl" raw_msg)
          results (keywordize-keys (get (json/read-str (neo4j/executeQuery cypher {"nodeid" (:nodeid raw_msg)})) "data"))]
      results)))

(defn clear-alerts [props]
  (with-open [_ (timers/start (metric-factory :timer
                                              ["clear-alerts"]))]
    (let [cypher (render-resource "cyphers/clear_alerts_for_node_name_type.cypher")
          result (keywordize-keys (json/read-str (neo4j/executeQuery cypher (stringify-keys props))))]
      (:alerts result []))))

(defn upsert-alert [props]
  (with-open [_ (timers/start (metric-factory :timer
                                              ["upsert-alert"]))]
    (let [cypher (render-resource "cyphers/upsert_alert_for_node_name_type.cypher")
          result (keywordize-keys (json/read-str (neo4j/executeQuery cypher (stringify-keys props))))]
      (:alerts result []))))

(defn uuid-from-nodeid [nodeid]
  (uuid/v3 (java.util.UUID/randomUUID) nodeid))

(defn get-clientid []
  (format "data-dealer-%s-%s" (Thread/currentThread) (str (java.util.UUID/randomUUID))))

(defn new-cfgreq-batch-uuids [nodeid]
  (format "v1/%s/in/va-v1/cfgreq/batchuuid" nodeid))

(defn new-cfgreq-batch-type [nodeid]
  (format "v1/%s/in/va-v1/cfgreq/batchtype" nodeid))

; Don't need this anymore, everything we need is in Cassandra
; Currently in here for repl debug usage
(defn send-all-type-request-kafka
  [nodeid]
  (let [rid (uuid-from-nodeid nodeid)
        jsonmsg (json/write-str (stringify-keys {:rid (str rid) :type "all"}))]
    (infof "Publishing all type request %s to kafka %s on %s" jsonmsg @kafka-consumer/device-topic-out (new-cfgreq-batch-type nodeid))
    (try
       (kafka/send-to-producer-kv @kafka-consumer/device-topic-out (new-cfgreq-batch-type nodeid) jsonmsg)
       (catch Exception e
         (errorf "send-all-type-request-kafka error: %s" e)
         ))
    ))

; Send to kafka instead with mqtt as key on the device-topic
(defn send-cfg-request-kafka
  [msg]
  (let [nodeid (:nodeid msg)
      rid (uuid-from-nodeid nodeid)
      uuids (:uuids msg)
      jsonmsg (json/write-str (stringify-keys {:rid (str rid) :uuids uuids}))]
    (when (< 0 (count uuids))
      (infof "Publishing config request to kafka %s" jsonmsg)
      (kafka/send-to-producer-kv @kafka-consumer/device-topic-out (new-cfgreq-batch-uuids nodeid) jsonmsg))))

(defn debug-encode-uuid [uuid]
  (let [x (Z85/encodeUUID uuid)]
    (if (= x "") (spy :debug (format "encodeUUID: %s -> %s" uuid x)))
    x))

(defn get-uuids-from-configs [nodeid siteid]
  (concat (map #(Z85/encodeUUID (:parkingzoneid %)) (actlogger/get_node_active_parking_zone_configs nodeid siteid))
          (doall (map #(Z85/encodeUUID (:eventid (:data %))) (actlogger/get_node_active_traffic_config nodeid)))))

(defn get-current-config [nodeid siteid]
  ;(spy :debug (get-uuids-from-configs nodeid siteid))
  (spy :debug (concat (actlogger/get_node_active_parking_zone_configs nodeid siteid)
          (doall (map #(:data %) (actlogger/get_node_active_traffic_config nodeid))))))

(comment (defn get-uuids-from-configs [configs]
  "Get all config uuids"
  (map #(:uuid (:h %)) configs)))

; Currently in here for repl debug usage
(defn send-batchuuid-request-kafka
  [nodeid siteid]
  (let [uuids (get-uuids-from-configs nodeid siteid)
        jsonmsg (json/write-str (stringify-keys {:nodeid nodeid :uuids uuids}))]
    (send-cfg-request-kafka jsonmsg)))

(defonce cache-known-nodes (atom {}))

(defn add-known-nodes [nodeid siteid]
  (swap! cache-known-nodes assoc (format "%s:%s" siteid nodeid) true))

(defn known-node? [nodeid siteid]
  (get @cache-known-nodes (format "%s:%s" siteid nodeid)))

(defn fetch-missing-config
  [nodeid siteid]
  (when (not= siteid "Unknown")
    (when (not (known-node? nodeid siteid))
      (add-known-nodes nodeid siteid)
      (info (format "fetch-missing-config: siteid %s, nodeid %s" siteid nodeid))
      (if (not (seq (get-current-config nodeid siteid)))
        (send-all-type-request-kafka nodeid)))))

(defmulti get-node-propmap :clientType)

(defmethod get-node-propmap "unode-v5"
  [{:keys [nodeid
           clientType
           swVerId
           netName
           profileName
           assocChannel
           configToken
           localIP
           time
           bssid
           subType
           voltageType]}]
  {:nodeid nodeid
   :model clientType
   :ip localIP
   :softwareVersion swVerId
   :configToken configToken

   :imei profileName
   :imsi netName
   :apn (str assocChannel)
   :iccid bssid
   :subType subType
   :voltageType voltageType})

(defmethod get-node-propmap "unode-v6"
  [{:keys [nodeid
           clientType
           swVerId
           netName
           profileName
           assocChannel
           configToken
           localIP
           time
           bssid
           subType
           voltageType
           modemRevEd]}]
  {:nodeid nodeid
   :model clientType
   :ip localIP
   :softwareVersion swVerId
   :configToken configToken

   :imei profileName
   :imsi netName
   :apn (str assocChannel)
   :iccid bssid
   :subType subType
   :voltageType voltageType
   :modemRevEd modemRevEd})

(defmethod get-node-propmap :default
  [{:keys [nodeid
           clientType
           swVerId
           netName
           configToken
           localIP
           bssid
           mac
           auth
           assocChannel
           subType
           voltageType]}]
  {:nodeid nodeid
   :model clientType
   :ip localIP
   :softwareVersion swVerId
   :configToken configToken
   :remoteNetwork netName
   :bssid bssid
   :mac mac
   :auth auth
   :channel (str assocChannel)
   :subType subType
   :voltageType voltageType})

(defn get-node-gps
  [{:keys [nodeid
           lat
           lon]}]
  {:nodeid nodeid
   :latitude_gps (str lat)
   :longitude_gps (str lon)})


(defn get-alarm-category
  [alarm-type]
  (get{
   "Disconnect" "Network"
   "CommFail" "Hardware"
   "SimFail" "Hardware"
   "NotTested" "Software"
   "DownrevSoftware" "Software"
   "BadSensorData" "InternalSensor"
   "ConfigFail" "Software"
   "DegradedNetwork" "Network"
   "SoftwareUpdateFail" "Software"
   "ScheduleFail" "Software"
   "PreRuninFail" "Software"
   "PostRuninFail" "Software"
   "USPFail" "Hardware"
   "PMACFail" "Hardware"
   "DriverFail" "Hardware"
   "FarmUSPFail" "Hardware"
   "SensorFail" "InternalSensor"
   "StrangeReboot" "Software"
   "Assert" "Software"
   "X509ClientFail" "Software"
   "X509ServerFail" "Software"
   "UnderPower" "Power"
   "OverPower" "Power"
   "HardFault" "Hardware"
   "HWFail_generic" "Hardware"
   "HWFail_HIH6131" "InternalSensor"
   "HWFail_ISL29023" "InternalSensor"
   "HWFail_SE95" "InternalSensor"
   "HWFail_ZMotion" "InternalSensor"
   "HWFail_MMA8451" "InternalSensor"
   "HWFail_TSC3414" "InternalSensor"
   "HWFail_UrbanUSP" "InternalSensor"
   "HWFail_RTC" "InternalSensor"
   "HWFail_EEPROM" "InternalSensor"
   "HWFail_NIGHTHAWK" "InternalSensor"
   "SWUpdateFail_SENSORPOD" "InternalSensor"
   "HWFail_STUCK_RELAY" "InternalSensor"
   "HWFail_PCT2075" "InternalSensor"
   "HWFAIL_SIHAWK" "InternalSensor"
   "HWFAIL_GPS" "InternalSensor"
   "HWFail_PodBus" "InternalSensor"
   "Epic_Fail" "Software"} alarm-type "Lighting"))

(defn backfill-missing-node-latlon
  "Given a Node message containing its GPS coordinates and nodeid, use
  GPS coordinates for user-facing coordinates when Neo4j's record for
  the Node is empty."
  [{:keys [nodeid
           latitude_gps
           longitude_gps]
    :as nodeprops}]
  (let [;; Fetch Neo4j's coordinates for the Node
        {:keys [lat
                lon]
         :as coordinates} (-> "cyphers/find_lat_lon.cypher"
                              io/resource
                              slurp
                              (neo4j/executeQuery {"nodeid" nodeid})
                              (json/read-str :key-fn keyword)
                              :node)
        ;; When either the latitude or longitude is invalid
        invalid-coordinates (or (clojure.string/blank? lat)
                                (clojure.string/blank? lon))
        has-site? (-> "cyphers/get_site_and_org_for_node.cypher"
                      io/resource
                      slurp
                      (neo4j/executeQuery {"nodeid" nodeid})
                      (json/read-str :key-fn keyword)
                      :data
                      :siteid)]
    (if (and has-site?
             invalid-coordinates)
      (merge nodeprops
             ;; Then copy the GPS coordinates as our own, by renaming
             ;; and merging.
             (cset/rename-keys nodeprops
                               {:latitude_gps :latitude
                                :longitude_gps :longitude}))
      nodeprops)))

(defn update-node-gps
  [msg]
  (let [{:keys [nodeid siteid orgid]} msg
        nodeprops (-> msg
                      get-node-gps
                      backfill-missing-node-latlon)
        message-map {:user "device"
                     :type "updateNode"
                     :nodeprops nodeprops
                     :orgprops {:orgid orgid}
                     :siteprops {:siteid siteid}}]
    ; Process the message in this thread
    (spy :debug (cape/update-node-on-login message-map))))

(defn update-node-props
  [msg]
  (let [{:keys [nodeid siteid orgid]} msg
        nodeprops (->> (get-node-propmap msg)
                       ;; Check if the value is non-nil
                       (remove (fn [[k v]]
                                 (or (nil? v)
                                     (and (string? v)
                                          (empty? v)))))
                       (into {}))
        message-map {:user "device"
                     :type "updateNode"
                     :nodeprops nodeprops
                     :orgprops {:orgid orgid}
                     :siteprops {:siteid siteid}}]
    ; Process the message in this thread
    (spy :debug (cape/update-node-on-login message-map))

    ))

(defn activate-node
  [msg]
  (let [nodeid (:nodeid msg)
        siteid (:siteid msg)
        orgid (:orgid msg)
        message-map {:user "device"
                     :type "activateNode"
                     :nodeprops {:nodeid nodeid}
                     :siteprops {:siteid siteid}}]
    (spy :debug (cape/db-selector (cape/template-selector message-map)))))

(defn create-empty-node
  ; expects the resolved message
  [msg]
  (let [nodeid (:nodeid msg)
        siteid (:siteid msg "Unknown")
        orgid (:orgid msg "Unknown")
        message-map {:user "device"
                     :type "createEmptyNode"
                     :nodeprops {:nodeid nodeid :model (:clientType msg)}}]

    ; Process the message in this thread
    (debugf "NEW NODEID %s" message-map)
    (spy :debug (cape/db-selector (cape/template-selector (cape/checkprops message-map))))
    ))

(defn send-schedule
  ; expects the resolved message
  [msg scheduleid]
  (let [{:keys [nodeid
                siteid
                orgid]
         :or {siteid "Unknown"
              orgid "Unknown"}} msg]

    ; Process the message in this thread
    (debugf "ASSIGNING SCHEDULE siteid %s scheduleid %s nodeid %s" siteid scheduleid nodeid)
    (schedules/send-loginreq-schedule siteid
                                      scheduleid
                                      nodeid
                                      false
                                      nil)))

(defn config-resp
  [msg]
  (let []

    ))

(defn connection-status
  [msg resolvednode]
  (let [nodeid (:nodeid msg)
        siteid (:siteid msg "Unknown")
        orgid (:orgid msg "Unknown")
        connected (= 0 (compare (:status msg) "connected"))
        alertid (str (java.util.UUID/randomUUID))
        name "DeviceAlarm"
        type "Disconnect"
        category (get-alarm-category type)
        severity "Minor"
        msg "Node Disconnected"
        alertprops {:alertid alertid
                    :msg msg
                    :type type
                    :severity severity
                    :nodeid nodeid
                    :name name
                    :siteid siteid
                    :orgid orgid
                    :category category
                    :sitename (:sitename resolvednode)
                    :nodename (:nodename resolvednode)
                    :orgname (:orgname resolvednode)
                    :siteaddress (:siteaddress resolvednode)
                    :bssid (:bssid resolvednode)
                    :nodehw (:nodehw resolvednode)}]

    (actlogger/log_connection_status nodeid connected)
    (if connected (actlogger/log_increment_connect_count nodeid))
    (actlogger/log_node_status {:nodeid nodeid :siteid siteid :orgid orgid :net_stat (if connected 1 0)})
    ;(actlogger/log_device_alarms {:nodeid nodeid :alarmtype type :severitycode severity :message msg :category category})
    (if-not connected
      ; Upsert a new alert on disconnect
      ;
      ;(upsert-alert alertprops)
      (kafka/kafka-alert (json/write-str {"name" "DeviceAlarm", "nodeid" nodeid, "alarmType" "Disconnect", "alarmSeverity" "Critical", "msg" "Node Disconnected"}))

      ; Clear all previous disconnect alerts on successful connection
      ;(map #(assoc % :severity "Clear") (flatten (clear-alerts alertprops)))
      (kafka/kafka-alert (json/write-str {"name" "DeviceAlarm","nodeid" nodeid, "alarmType" "Disconnect", "alarmSeverity" "Clear", "msg" "Node Connected"})))))

;; TODO: On login, send the schedule for "today", as according to the
;; site. (Mind that international date line!)
(defn login-req
  [msg resolvednode]
  (let [{:keys [nodeid scheduleid configid model]
         :or {scheduleid "default"
              configid "default"}} resolvednode]

    ; Node exists
    (when nodeid
      ; Refresh the db with latest node values from loginreq
      (myanotime (update-node-props msg) "updateNode"))

    ; Node does not exist in platform
    (when-not nodeid
      ; Create it
      (myanotime (create-empty-node msg) "createEmptyNode"))

    ;; Send after Schedule, as Schedule sending appears to clear all
    ;; overrides.
    (let [;; pull last override from db
          ;; update `system` to req on cassandra/metrics?
          {:keys [driver
                  isscheduled
                  startdt
                  priority]
           :as light-mode} (some-> nodeid
                                   actlogger/get_latest_light_mode)
          timeout (if startdt
                    (/ (- (.getTime startdt)
                          (.getMillis (time-core/now)))
                       1000)
                    0)]

      ;; Send the schedule before sending config.
      ;; Config will potentially cause a reboot on change
      (myanotime (send-schedule msg scheduleid) "applyScheduleToNode")

      (when (and nodeid
                 (not (empty? light-mode)))
        ;; Check for explicit `false`, not `nil`.
        (when (and (false? isscheduled) (pos? timeout))
          (dev-ctrl/query-exec-msgpk {:nodeprops {:type "LightingForceState"
                                                  :nodeid [nodeid]
                                                  :level driver
                                                  :timeout timeout
                                                  :pri priority}}))))

    (myanotime (cape/send-config-to-node msg) "send-config-to-node")))

(defn device-alarm
  [msg]
  (let [orgid (:orgid msg)
        nodeid (:nodeid msg)
        siteid (:siteid msg)
        alarmType (:alarmType msg)
        alarmName (:name msg)
        alarmSeverity (:alarmSeverity msg)
        category (get-alarm-category alarmType)
        alarmmsg (:msg msg)
        alertid (str (java.util.UUID/randomUUID))
        alarmdata {:nodeid nodeid :alarmtype alarmType :severitycode alarmSeverity :message alarmmsg :category category}
        alertprops {:alertid alertid :msg alarmmsg :type alarmType :severity alarmSeverity :category category :nodeid nodeid :name alarmName :siteid siteid :orgid orgid}
        clear (= 0 (compare alarmSeverity "Clear"))
        alerts (if (and (= orgid "Unknown") (= siteid "Unknown"))
          [alertprops]
          (if clear (clear-alerts alertprops) (upsert-alert alertprops)))]
    (actlogger/log_device_alarms  alarmdata)
    alerts))

(defn convert-sensor-sample-value
  [sensor value]
  {:pre [value]}
  (case sensor
    "l" (float (/ value 1000))
    "l-i" (float (/ value 1000))
    "lIR" (float (/ value 1000))
    ; HoneyT conversion
    ; http://sensing.honeywell.com/honeywell-sensing-humidicon-hih6100-series-product-sheet-009059-6-en.pdf
    ;  ( T / 2^14 - 2 ) * 165 - 40
    "t" (float (- (* 0.01007203 value) 40))
    "T" (float (/ value 1000))
    "v" (float (/ value 1000))
    "vp" (float (/ value 1000))
    "mi" (float (/ value 1000))
    "mip" (float (/ value 1000))
    "i" (float (/ value 1000))
    "ai" (float (/ value 1000))
    "aip" (float (/ value 1000))
    "w" (float (/ value (* 1000 3600)))
    "mw" (float (/ value (* 1000 3600)))
    "aw" (float (/ value (* 1000 3600)))
    "mP" (float (/ value 1000))
    "aP" (float (/ value 1000))
    "PF" (float (/ value (Math/pow 2 30)))
    "mPF" (float (/ value (Math/pow 2 30)))
    "aPF" (float (/ value (Math/pow 2 30)))
    "jtx" (float (/ value (Math/pow 2 14)))
    "jty" (float (/ value (Math/pow 2 14)))
    "jtz" (float (/ value (Math/pow 2 14)))
    "jtm" (float (/ value (Math/pow 2 14)))
    value))

(defn log-sensor-value
  [sampledata]
  (debug sampledata)
  (ac/put-sensor-sample sampledata)
  ;(actlogger/log_sensor_sample sampledata)
  sampledata)

(defn log-parking-current-status-value
  [sampledata]
  ;(spy :debug sampledata)
  (actlogger/log_parking_current_status  sampledata))

(defn log-parking-historic
    [sampledata]
    ;(spy :debug sampledata)
    (actlogger/log_parking_historic  sampledata))

(defn handle-sensor-sample
  [sampledata]
  (let [sensor (:sensor sampledata)
        value (:value sampledata)
        updatedvalue (convert-sensor-sample-value sensor value)
        updatedsampledata (merge sampledata {:value updatedvalue})]
    (log-sensor-value (dissoc updatedsampledata :values))))

(defn handle-rf-sensor-sample
  [sampledata]
  (let [nodeid (:nodeid sampledata)
        time (:time sampledata)
        values (:values sampledata)]
    (doall (map (fn [value sensor]
                  (log-sensor-value {:nodeid nodeid
                                     :sensor sensor
                                     :value value
                                     :time time}))
                values
                ["RF" "sn"]))))

(defn handle-jt-sensor-sample
  [sampledata]
  (let [{:keys [nodeid
                time
                values]
         [jtx jty jtz] :values} sampledata]
    (if (and (every? some? [jtx jty jtz])
             (not-any? zero? [jtx jty jtz]))
      (let [[jtx jty jtz] (map #(convert-sensor-sample-value %1 %2)
                               ["jtx" "jty" "jtz"]
                               [jtx jty jtz])
            jtm (Math/sqrt (+ (* jtx jtx)
                              (* jty jty)
                              (* jtz jtz)))
            updatedvalues [jtx jty jtx jtm]]
        (doall (map (fn [value sensor]
                      (log-sensor-value {:nodeid nodeid
                                         :sensor sensor
                                         :value value
                                         :time time}))
                    updatedvalues
                    ["jtx" "jty" "jtz" "jtm"])))
      (warn (ex-info "Received malformed jolt."
                     {:nodeid nodeid
                      :time time
                      :values values})))))

(defn handle-br-sensor-sample
  [sampledata]
  (let [nodeid (:nodeid sampledata)
        time (:time sampledata)
        values (:values sampledata)]
    (doall (map (fn [value sensor]
                  (log-sensor-value {:nodeid nodeid
                                     :sensor sensor
                                     :value value
                                     :time time}))
                values
                ["bRL" "bRA"]))))

(defn convertUUID [uuid]
  (try
    (let [padding-required (- 5
                              (rem (count uuid)
                                   5))
          padding (apply str (repeat padding-required
                                     ;; The character to pad encoded z85 with is `u`.
                                     ;; See: https://en.wikipedia.org/wiki/Ascii85
                                     "u"))
          z85uuid (str uuid padding)]
      (if (> (count z85uuid) 19)
        (Z85/decodeUUID z85uuid)
        (debug "Missing UUID")))
    (catch Throwable t
      (errorf "Cannot convert UUID: %s"
              uuid)
      (error t))))

(def microsecond-last-year
  (-> 1
      time-core/years
      time-core/ago
      c/to-long
      (* 1000)))

(defn log-sensor-sample
  [{:keys [time
           nodeid
           sensor]
    :as sampledata}]
  (debug sampledata)
  (if (< time microsecond-last-year)
    (do
      ; This fills up graphite quickly...
      ;(histograms/update! (metric-factory :histogram
      ;                                    ["timetraveler"
      ;                                     nodeid
      ;                                     sensor])
      ;                    time)
      (warn (ex-info "Received sensor sample timestamped over a year old."
                     sampledata)))
    ; Don't need to convert anymore, just log it.
    (log-sensor-value sampledata)
    ;(case sensor
    ;  "rf" (handle-rf-sensor-sample sampledata)
    ;  "jt" (handle-jt-sensor-sample sampledata)
    ;  "bR" (handle-br-sensor-sample sampledata)
    ;  (handle-sensor-sample sampledata))
    ))

(defn sensor-sample
  ; "jt"  Jolt Thresho
  ; "mt"  Node microcontroller temperature (degC)
  ; "l"   Ambient Light
  ; "lIR" Infrared Sensor
  ; "t"   Internal Temperature
  ; "p"   Presence Sensor
  ; "pc"  Presence Counter
  ; "T"   Ambient Temperature (mC)
  ; "v"   Voltage (mV)
  ; "vp"  Main/Aux Voltage Peak (mV)
  ; "mi"  Main Current (mA)
  ; "mip" Main Peak Current
  ; "ai"  Auxiliary Current (mA)
  ; "aip" Auxiliary Peak Current
  ; "mw"  Main Power (mW)
  ; "aw"  Aux Channel Energy Used (mW)
  ; "mP"  Main Power (mW)
  ; "aP"  Aux Power (mW)
  ; "mPF" Main Power Factor
  ; "aPF" Auxiliary Power Factor
  ; "lt"  Light Driver Level
  ; "rf"  Received Signal Strength
  ; "bc"  Boot Count
  ; "WDT" Watchdog Timer
  ; "bR"  Boot Reason
  [msg resolvednode]
  (let [nodeid (:nodeid msg)
        sensor (:sensor msg)
        nodelabels (:nodelabels resolvednode)
        sitelabels (:sitelabels resolvednode [])
        scheduleid (:scheduleid resolvednode)
        configid (:configid resolvednode)
        value (double (:value msg))
        ;values (:values msg)
        time (bigint (:time msg))
        sampledata {:nodeid nodeid :sensor sensor :value value ;:values values
                    :time time}
        logged-value (log-sensor-sample sampledata)]
;    (when (= sensor "lt")
;      (actlogger/log_connection_status nodeid true)
;      (actlogger/log_node_status {:nodeid nodeid
;                                  :siteid (:siteid msg "Unknown")
;                                  :orgid (:orgid msg "Unknown")
;                                  :net_stat 1
;                                  :sen_stat (str value)
;                                  :lig_stat (if (> (int value) 0) "on" "off")}))
    (spy :debug (format "SensorSample for node '%s' node labels: %s  site labels: %s schedule id %s config id %s" nodeid nodelabels sitelabels scheduleid configid))
    (if (and (some #(= "Inactive" %) nodelabels)
             (some #(= "Assigned" %) nodelabels)
             (some #(= "Active" %) sitelabels)
             (not (nil? scheduleid))
             (not (nil? configid)))
      ; If node is assigned, inactive, has a schedule and has a config => activate
      (activate-node msg))
    logged-value))

(defn resolve-parking [raw_msg psid oid ts]
  (let [nodeid (:nodeid raw_msg)
        siteid (:siteid raw_msg)
        orgid (:orgid raw_msg)
        pgid (:parkinggroupid raw_msg)
        parkingspotid (convertUUID psid)
        parkingzoneid (convertUUID (:uuid (:h raw_msg)))
        occupancy     (not= oid "")
        objectid      (convertUUID oid)
        channel (:ch (:h raw_msg))
        active (:e (:h raw_msg))
        since (bigint ts)
        results {:nodeid nodeid :orgid orgid :siteid siteid :parkingzoneid parkingzoneid
          :parkinggroupid pgid :parkingspotid parkingspotid :channel channel
          :occupancy occupancy :objectid objectid
          :active active :since since :demarcated true}
    ] results ))

(defn resolve-empty-parking [raw_msg psid oid ts]
  (let [nodeid (:nodeid raw_msg)
        siteid (:siteid raw_msg)
        orgid (:orgid raw_msg)
        pgid (:parkinggroupid raw_msg)
        parkingspotid psid
        parkingzoneid (convertUUID (:uuid (:h raw_msg)))
        occupancy     (not= oid "")
        objectid      (convertUUID oid)
        channel (:ch (:h raw_msg))
        active (:e (:h raw_msg))
        since (bigint ts)
        results {:nodeid nodeid :orgid orgid :siteid siteid :parkingzoneid parkingzoneid
          :parkinggroupid pgid :parkingspotid parkingspotid :channel channel
          :occupancy occupancy :objectid oid
          :active active :since since :demarcated true}
    ] results ))

(defn resolve-parking-config [raw_msg psid pzid active x1 x2 x3 x4 y1 y2 y3 y4 lat1 lat2 lat3 lat4 lng1 lng2 lng3 lng4]
  (let [nodeid (:nodeid raw_msg)
        siteid (:siteid raw_msg)
        orgid (:orgid raw_msg)
        ts (:t (:h raw_msg))
        parkingspotid (convertUUID psid)
        parkingzoneid (convertUUID pzid)
        activesince ts
        results { :nodeid nodeid :orgid orgid :siteid siteid :parkingzoneid parkingzoneid :parkingspotid parkingspotid
          :x1 x1 :x2 x2 :x3 x3 :x4 x4 :y1 y1 :y2 y2 :y3 y3 :y4 y4
          :lat1 lat1 :lat2 lat2 :lat3 lat3 :lat4 lat4
          :lng1 lng1 :lng2 lng2 :lng3 lng3 :lng4 lng4
          :activesince activesince :demarcated true :active active}
    ] results ))

(defn resolve-nondemarcated-parking [raw_msg psid pzid ts x1 x2 y1 y2 lat1 lng1 lat2 lng2 lat3 lng3 lat4 lng4 pgid oid occupancy]
  (let [nodeid (:nodeid raw_msg)
        channel (:ch (:h raw_msg))
        active true
        siteid (:siteid raw_msg)
        orgid (:orgid raw_msg)
        results { :parkingzoneid (convertUUID pzid) :parkingspotid (convertUUID psid)
          :x1 x1 :x2 x2 :y1 y1 :y2 y2 :occupancy occupancy :nodeid nodeid
          :lat1 lat1 :lng1 lng1 :lat2 lat2 :lng2 lng2 :lat3 lat3 :lng3 lng3 :lat4 lat4 :lng4 lng4 :since ts :channel channel
          :active active :demarcated false :siteid siteid :orgid orgid :objectid (convertUUID oid) :parkinggroupid pgid}
    ] results ))

; More details: https://xeranet.atlassian.net/wiki/display/KB/Micronode+Sensor+and+Unit+Names
(defn resolve-units [raw_msg]
  (let [sensor (:sensor raw_msg)
         units ( case sensor
                 "lt"  "%" ; Light Driver Level
                 "l"   "lux" ; Ambient Light
                 "l-i" "lux"
                 "lIR" "lux" ; Infrared Sensor
                 "p"   "" ; Presence Sensor
                 "pc"  "" ; Presence Counter
                 "pdc"  "" ; Presence Zmotion DC register
                 "ppr"  "" ; Presence Zmotion PR register
                 "pnd"  "" ; Presence Zmotion ND register
                 "pdt"  "" ; Presence Zmotion DT register
                 "t"   "degC" ; Internal Temperature
                 "T"   "degC" ; Ambient Temperature (mC)
                 "mt"  "degC" ; Node microcontroller temperature (degC)
                 "rf"  "RFsn" ; Received Signal Strength

                 "RF"  "dBm" ; Received Signal Strength
                 "sn"  "dB"  ; Noise Figure

                 "bc"  "" ; Boot Count
                 "WDT" "" ; Watchdog Timer
                 "bR"  "" ; Boot Reason
                 "jt"  "JTxyzm" ; Jolt Threshold
                 "v"   "V" ; Voltage (mV)
                 "vp"  "V" ; Main/Aux Voltage Peak (mV)
                 "i"  "A" ; Main Current (mA)
                 "mi"  "A" ; Main Current (mA)
                 "mip" "A" ; Main Peak Current
                 "ai"  "A" ; Auxiliary Current (mA)
                 "aip" "A" ; Auxiliary Peak Current
                 "w"  "WH" ; Main Power (mW)
                 "mw"  "WH" ; Main Power (mW)
                 "aw"  "WH" ; Aux Channel Energy Used (mW)
                 "mP"  "W" ; Main Power (mW)
                 "aP"  "W" ; Aux Power (mW)
                 "PF" "PF30" ; Main Power Factor
                 "mPF" "PF30" ; Main Power Factor
                 "aPF" "PF30" ; Auxiliary Power Factor
                 ""
           )
         results (assoc raw_msg :units units )
         ; log (spyf :info "Fixed units for sensor samples: %s" results)
    ] results ))

;Convert Date to string
(defn as-time-string [date]
  (f/unparse (f/formatter "yyyy-MM-dd HH:mm:ss.SS") date))
;Used for json/write-str for parking and traffic events
(defn time-aware-value-writer [key value]
  (if (or (= key :configured_date) (= key :time)) (as-time-string value) value))

(defn compare-zones [zones pzid]
  (let [splitstr (clojure.string/split (get zones :zones "") #" ")
        result (.indexOf splitstr pzid)]
  result))

(defn resolve-parkinggroup [pgs pzid]
  (let [pgid (filter (fn [group] (not= (compare-zones group pzid) -1)) pgs)
    ret (get (first pgid) :pgid "Unknown")]
    ret))

; Make vector of lat/lon par vectors
(defn join_lats_lons [world_coordinates]
  (let[
        lats (:lat world_coordinates)
        lons (:lon world_coordinates)
        maxcnt (count lats)
        result []
        ]
    (loop [i 0 result []]
      (if (< i maxcnt)
          (let[
                lat (nth lats i)
                lon (nth lons i)
                ]
            (recur (inc i)
                   (conj result {:latitude lat :longitude lon})))
          result))
    ))

(defn resolve_lats_lons [list]
  (let[
        maxcnt (count list)
        result []
        ]
    (loop [i 0 result []]
      (if (< i maxcnt)
          (let[
                item (nth list i)
                resolved (join_lats_lons item)
                ]
            (recur (inc i)
                   (conj result resolved)))
          result))
    ))

; Make vector of x/y par vectors
(defn join_xs_ys [img_coordinates]
  (let[
        xs (:v img_coordinates)
        ys (:u img_coordinates)
        maxcnt (count xs)
        result []
        ]
    (loop [i 0 result []]
      (if (< i maxcnt)
          (let[
                x (nth xs i)
                y (nth ys i)
                ]
            (recur (inc i)
                   (conj result {:x x :y y})))
          result))
    ))
(defn resolve_xs_ys [list]
  (let[
        maxcnt (count list)
        result []
        ]
    (loop [i 0 result []]
      (if (< i maxcnt)
          (let[
                item (nth list i)
                resolved (join_xs_ys item)
                ]
            (recur (inc i)
                   (conj result resolved)))
          result))
    ))


; TODO Make vector of lat/lon par vectors
(defn resolve_traffic_points [world_coordinates]
  [["TODO-UUID-1" "TODO-UUID-2"]]
  )
; TODO Make vector of lat/lon par vectors
(defn resolve_area_type [type]
  "line")

(declare process-device-msg)

; Process deleted config:
; - delete from current_state,
; - disable from configs
(defn handle-deleted-config [pkzid eventid name siteid]
  (case name

    ("NonDemarcatedParkingConfig" "DemarcatedParkingConfig") (actlogger/log_parking_zone_removed siteid pkzid)
    ("ObjectLeavingConfig"
      "ObjectDwellConfig"
      "ObjectEnteringConfig"
      "LineCrossingConfig") (do
                              (actlogger/log_traffic_config_removed eventid)
                              (actlogger/log_traffic_current_removed eventid))
    nil
    ))

(defn process-all-config [msg]
  (spyf :debug "ProcessAllConfig Processing: %s" msg)
  (let [nodeid (:nodeid msg)
        newconf (:cfgs msg)
        ;newconf (:current msg)
        ;prevconf (:prev msg)
        siteid (:siteid msg)
        prevconf (get-current-config nodeid siteid)
        maxcntprv (count prevconf)
        eventids (vec (for [rec newconf,
                       :let [
                              type (:type rec)
                              cfg (:cfg rec)
                              eventid (convertUUID (:uuid (:h cfg)))
                              name (case type
                                     "ndpark" "NonDemarcatedParkingConfig"
                                     "dpark" "DemarcatedParkingConfig"
                                     "linec" "LineCrossingConfig"
                                     "objent" "ObjectEnteringConfig"
                                     "objlev" "ObjectLeavingConfig"
                                     "objdwl" "ObjectDwellConfig"
                                     "ObjectEntered" "ObjectEnteringConfig"
                                     "ObjectExit" "ObjectLeavingConfig"
                                     "")
                              newmsg (merge cfg {:nodeid nodeid :name name})]
                       :when (not= name "")]
                   (do
                     (process-device-msg newmsg)
                     (spyf :debug (str "ProcessAllConfig Found "  type name ": %s") newmsg)
                     eventid)))
        ]

    (spyf :debug (str "ProcessAllConfig Prev configs "  (count eventids) ": %s") eventids)
    ; Find deleted configs
    (dotimes [i maxcntprv]
      (let [
             rec (nth prevconf i)
             ;cfg (:cfg rec)
             type (:type rec)
             eventid (:eventid rec)
             pkzid (:parkingzoneid rec)
             #_(name (case type
                   "ndpark" "NonDemarcatedParkingConfig"
                   "dpark" "DemarcatedParkingConfig"
                   "linec" "LineCrossingConfig"
                   "objent" "ObjectEnteringConfig"
                   "objlev" "ObjectLeavingConfig"
                   "objdwl" "ObjectDwellConfig"
                   "ObjectEntered" "ObjectEnteringConfig"
                   "ObjectExit" "ObjectLeavingConfig"
                   ""))]
        (if (and (not= name "") (not (some #(= eventid %) eventids ))) (do
                             (spyf :debug (str "ProcessAllConfig Deleted " type " Found "  eventid ": %s") eventids)
                             (handle-deleted-config pkzid eventid type siteid)))))
  ))

(defn resolve-nondem-spots [spots]
  (map #(update %
                :uuid convertUUID)
       spots))

(defn make-timestamps
  [ts]
  (if (nil? ts)
    nil
    (as-time-string (c/from-long (quot ts 1000)))))

; name doesn't work if it has a slash in it, need to convert to string and remove ":" at beginning
(defn keyword->string [k]
  (subs (str k) 1))

(defn spotuuids [occ]
  (map keyword->string (keys occ)))

(defn objectuuids [occ]
  (vals occ))

(defn resolve-spots [spots]
 (let [
       objects (:o spots)
       ;ndspots (spotuuids (:occ spots))
       ndspots (:spotuuids (:occ spots))
       ;ndobjects (objectuuids (:occ spots))
       ndobjects (:objectuuids (:occ spots))
       updatedspots (map convertUUID ndspots)
       updatedobjectuuids (map #(-> %
                                    first
                                    convertUUID)
                               ndobjects)
       updatedobjects1 (map #(update % :uuid convertUUID) objects)
       updatedobjects (map #(update % :s make-timestamps) updatedobjects1)
       result {:occ {:spotuuids updatedspots
                     :objectuuids updatedobjectuuids
                     }
               :o updatedobjects}]
   result))

(defn resolve-conf-spots [spots]
(let [
      updatedspots (map #(update-in % [:uuid] convertUUID) spots)]
  updatedspots))

(defn resolve-roi [obj]
  (if (= obj nil) nil
    (let[
          roiid (convertUUID (:uuid obj))
          name (:n obj)
          img (:img obj)
          world (:world obj)
          vs (:vs obj)
          image_bounding_box (resolve_xs_ys img)
          world_bounding_box (resolve_lats_lons world)
          ]
      {:roiid roiid
       :name name
       :image_bounding_box image_bounding_box
       :world_bounding_box world_bounding_box
       :vs vs
       })))

(defn mqtt-publish-parking [raw_msg spots subtopic clientid]
  (let [name (:name raw_msg)
        nodeid (:nodeid raw_msg)
        siteid (:siteid raw_msg)
        orgid (:orgid raw_msg)
        is_config (if (= -1 (.indexOf name "Config")) false true)
        eventtopic (format "/streamv1/%s/%s/%s/parking/%s" orgid siteid nodeid (if is_config "ParkingConfigEvent" "ParkingEvent"))
        header (:h raw_msg)
        pzid (convertUUID (:uuid header))
        desc (:des header)
        roi (:roi raw_msg)
        type (:n header)
        ename (:n header)
        channel (:ch header)
        active (:e header)
        tag (:tag header)
        ts (:t header)
        time (c/from-long (quot ts 1000))
        resolvedmsg (merge {
                            :subtopic subtopic
                            :spots (if (#{"DemarcatedParkingEvent"} name)
                                     (resolve-spots spots)
                                     (if (#{"DemarcatedParkingConfig"} name) (resolve-conf-spots spots) (resolve-nondem-spots spots)))
                            :type name
                            :name ename
                            :orgid orgid
                            :siteid siteid
                            ;:configured_date time
                            ;:tag tag
                            :description desc
                            ;:roi (resolve-roi roi)
                            :active active
                            :channel channel
                            :nodeid nodeid
                            :parkingzoneid pzid
                            ;:image_coordinates (join_xs_ys (:img area))
                            ;:world_coordinates (join_lats_lons (:world area))
                            }
                           (if-not (.contains name "Config")
                             {:time time}
                             {:configured_date time :roi (resolve-roi roi) :tag tag}))
        jsonmsg (json/write-str resolvedmsg :value-fn time-aware-value-writer)]
    ;(spyf :error "Got parking event: %s" jsonmsg)
    (mqtt-publish eventtopic jsonmsg clientid)))

(defn resolve-objects [objects]
  (let[
        result []
        ]
      (loop [i 0 result []]
        (if (< i (count objects))
            (let [
                   obj (spy :debug (nth objects i))
                   objid (convertUUID (:uuid obj))
                   position_precision (:wp obj)
                   height (:wh obj)
                   obj_image_velocity (:iv obj)
                   obj_velocity (:wv obj)
                   color (:co obj)
                   class (:c obj)
                   image_velocity {
                                    :x (nth obj_image_velocity 0)
                                    :y (nth obj_image_velocity 1)
                                    }
                   velocity  {
                               :x (nth obj_velocity 0)
                               :y (nth obj_velocity 1)
                               }
                   image_bounding_box (join_xs_ys (:img obj))
                   world_bounding_box (join_lats_lons (:world obj))
                   ]
              (recur (inc i) (conj result (spy :debug {
                                                        :detectedobjectid objid
                                                        :position_precision position_precision
                                                        :height height
                                                        :class class
                                                        :color color
                                                        :image_bounding_box image_bounding_box
                                                        :world_bounding_box world_bounding_box
                                                        :image_velocity image_velocity
                                                        :world_velocity velocity
                                                        })))
              )
            (spy :debug result)))
      )
    )

(defn log-and-publish-traffic-event [raw_msg resolvednode clientid]
  (let  [
          name (:name raw_msg)
          nodeid (:nodeid raw_msg)
          count (:c raw_msg)
          countpc (:co raw_msg)
          objects (:o raw_msg)
          header (:h raw_msg)
          eventid (convertUUID (:uuid header))
          eventname (:n header)
          channel (:ch header)
          enabled (:e header)
          ts (:t header)
          human_ts (c/from-long (quot ts 1000))
          orgid (:orgid resolvednode (:orgid raw_msg "Unknown"))
          siteid (:siteid resolvednode (:siteid raw_msg "Unknown"))
          eventtopic (format "/streamv1/%s/%s/%s/%s/%s" orgid siteid nodeid "traffic" "TrafficDetectionEvent")
          detected_objects  (if (= objects "") [] (resolve-objects objects))
          current_status {
                           :time human_ts
                           :trafficdetectioneventid eventid
                           :name eventname
                           :orgid orgid
                           :siteid siteid
                           :nodeid nodeid
                           :type name
                           :active enabled
                           :channel channel
                           :count count
                           :count_per_class countpc
                           :detected_objects detected_objects
                           }
          logdata {
                    :time ts
                    :trafficdetectioneventid eventid
                    :name eventname
                    :orgid orgid
                    :siteid siteid
                    :nodeid nodeid
                    :type name
                    :active enabled
                    :channel channel
                    :count count
                    :data (json/write-str current_status :value-fn time-aware-value-writer)  ;; serialized current status
                    }
          jsonmsg (json/write-str current_status :value-fn time-aware-value-writer)
          ]
    (do
      ;(spyf :debug "Got traffic event: %s" jsonmsg)
      ;; Publish event to mqtt (for SSE)
      (if enabled (mqtt-publish eventtopic jsonmsg clientid))
      ;; Log status
      (actlogger/log_traffic_current_status logdata)
      )))

(defn log-and-publish-traffic-config [raw_msg resolvednode clientid]
  (let  [
          name (:name raw_msg)
          nodeid (:nodeid raw_msg)
          header (:h raw_msg)
          roi (:roi raw_msg)
          tr (:tr raw_msg)
          dwell_time (:dt raw_msg)
          direction_tolerance (:wd raw_msg)
          eventid (convertUUID (:uuid header))
          eventname (:n header)
          eventdescription (:des header)
          channel (:ch header)
          enabled (:e header)
          ts (:t header)
          tag (:tag header)
          user (:u header)
          human_ts (c/from-long (quot ts 1000))
          orgid (:orgid resolvednode (:orgid raw_msg "Unknown"))
          siteid (:siteid resolvednode (:siteid raw_msg "Unknown"))
          eventtopic (format "/streamv1/%s/%s/%s/%s/%s" orgid siteid nodeid "traffic" "TrafficConfigEvent")
          ;removed (:removed raw_msg)
          ;maxremoved (count removed)
          config_data (merge {
                     :configured_date human_ts
                     :eventid eventid
                     :name eventname
                     :description eventdescription
                     :orgid orgid
                     :siteid siteid
                     :nodeid nodeid
                     :type name
                     :active enabled
                     :channel channel
                     :tag tag
                     :user user
                     :roi (resolve-roi roi)
                     }
                     (if (nil? dwell_time) nil {:dwell_time dwell_time})
                     (if (nil? tr) nil {:tr tr})
                     (if (nil? direction_tolerance) nil {:direction_tolerance direction_tolerance}))
          logdata {
                    :time ts
                    :eventid eventid
                    :name eventname
                    :orgid orgid
                    :siteid siteid
                    :nodeid nodeid
                    :type name
                    :active enabled
                    :channel channel
                    :data (json/write-str config_data :value-fn time-aware-value-writer)  ;; serialized current status
                    }
          jsonmsg (json/write-str config_data :value-fn time-aware-value-writer)
          ]
    (do
      (spyf :debug "GotTrafficConfig raw: %s" raw_msg)
      (spyf :debug "GotTrafficConfig json: %s" jsonmsg)
      ;; Publish event to mqtt (for SSE)
      (mqtt-publish eventtopic jsonmsg clientid)
      ;; Log current status
      (actlogger/log_traffic_config logdata)
      ;; This is a noop, has no data from DCC ever.
      ;; Remove all removed
      ;(dotimes [j maxremoved]
      ;  (actlogger/log_traffic_config_removed (nth removed j)))
      )))

(def delta-timer (atom 0))

(defn get-removed-spots [pkzid spots]
  (let [prev-spots (map #(:parkingspotid %) (actlogger/get_current_spots pkzid))
        cur-spots (map #(convertUUID (:uuid %)) spots)]
    (seq (cset/difference (set prev-spots) (set cur-spots)))))

(defn process-device-msg [mpmsg]
  (with-open [_ (timers/start (metric-factory :timer
                                              [devsvcworker-timer-name]))]
    (try
                                        ;(spyf :debug (str "GotSomething Is byte-array: %s")  (bytes? mpmsg))
      (let [raw_msg (if (bytes? mpmsg) (keywordize-keys (spy :debug (msgpk/unpack mpmsg))) mpmsg)
            {:keys [name nodeid]} raw_msg
            parkingevent (#{"DemarcatedParkingEvent"
                            "DemarcatedParkingConfig"
                            "NonDemarcatedParkingEvent"
                            "NonDemarcatedParkingConfig"}
                          name)
            trafficevent (#{"LineCrossingConfig" "ObjectEnteringConfig" "ObjectLeavingConfig" "ObjectDwellConfig" "LineCrossingEvent" "ObjectEnteringEvent" "ObjectLeavingEvent" "ObjectDwellEvent"} name)
            resolvednode (if parkingevent
                           (resolve-video-node raw_msg)
                           (resolve-node raw_msg))
            {:keys [orgid siteid]} (merge {:orgid "Unknown"
                                           :siteid "Unknown"}
                                          (if (nil? raw_msg) {} raw_msg)
                                          (if (nil? resolvednode) {} resolvednode))
            parkingzoneid (if parkingevent (:uuid (:h raw_msg) ) "")
            parkinggroups (if parkingevent
                            (:parkinggroups resolvednode "Unknown")
                            "")
            parkinggroupid (if parkingevent
                             (resolve-parkinggroup parkinggroups (:uuid (:h raw_msg)))
                             "")
            clientid (get-clientid)
            resolvedmsg (if parkingevent
                          (assoc raw_msg
                                 :orgid orgid
                                 :siteid siteid
                                 :parkinggroupid parkinggroupid
                                 :parkingzoneid parkingzoneid)
                          (assoc raw_msg
                                 :orgid orgid
                                 :siteid siteid))
            topic (format "/streamv1/%s/%s/%s/%s" orgid siteid nodeid name)
            jsonmsg (json/write-str resolvedmsg)
            log (format "%s topic %s msg %s clientid %s" name topic jsonmsg clientid)]
        (debugf "Received request: %s" raw_msg)
        (if (or trafficevent parkingevent) (fetch-missing-config nodeid siteid))
        (with-open [_ (timers/start (metric-factory :timer
                                                    [devsvcworker-timer-name (str name)]))]
          (debug log)
          (case name
            "OTAStatus"
            (let [jobid (:jobid raw_msg)
                  {:keys [siteid orgid target_type target_id model node_count]}
                    (into {} (actlogger/get_org_site_from_ota_log jobid))
                  topic (format "/streamv1/%s/%s/%s/%s" orgid siteid jobid name)
                  extended_map {:firmwareid (:firmwareid raw_msg)
                                :jobid jobid
                                :status (:status raw_msg)
                                :success (:success raw_msg)
                                :description (:description raw_msg)
                                :nodeid nodeid
                                :orgid orgid
                                :siteid siteid
                                :target_type target_type
                                :target_id target_id
                                :model model
                                :node_count node_count
                                :when (:when raw_msg)}]
              (actlogger/log_ota extended_map)
              (mqtt-publish topic (json/write-str extended_map) clientid))
            "ConfigResp"
            (do
              (config-resp resolvedmsg)
              (mqtt-publish topic jsonmsg clientid))

            "ConnectionStatus"
            (do
              ;(let [alarmtopic (format "/streamv1/%s/%s/%s/%s" orgid siteid nodeid "DeviceAlarm")
              ;      alerts (myanotime (connection-status resolvedmsg resolvednode) "connection-status")]
              ;  (dorun (map #(mqtt-publish alarmtopic (json/write-str %) clientid) alerts)))
              (myanotime (connection-status resolvedmsg resolvednode) "connection-status")
              (mqtt-publish topic jsonmsg clientid))

            "LoginReq"
            (do
              (login-req resolvedmsg resolvednode)
              ; We no longer need to do this after ConnectionStatus `connected` add to Legacy
              ;(dev-ctrl/query-exec-msgpk {:nodeprops {:type "SensorSampleReq"
              ;                                        :nodeid [(:nodeid resolvednode)]
              ;                                        }
              ;                            :extprops {:sensor "lt"}
              ;                           })
              (mqtt-publish topic jsonmsg clientid))

            "SensorSample"
            (sensor-sample resolvedmsg resolvednode)

            "DemarcatedParkingEvent"
            (if (not= (:siteid resolvedmsg) "Unknown")
              (let[
                   objs (:o resolvedmsg)
                   ;occ (:occ resolvedmsg)
                   ;roi (:roi resolvedmsg)
                   suuids (spotuuids (:occ resolvedmsg))
                   ;suuids (:spotuuids (:occ resolvedmsg))
                   maxcnt (count suuids)
                   ouuids (objectuuids (:occ resolvedmsg))
                   ;ouuids (:objectuuids (:occ resolvedmsg))
                   spots {:occ {:spotuuids suuids :objectuuids ouuids} :o objs}
                   nodeid (:nodeid resolvedmsg)
                   siteid (:siteid resolvedmsg)
                   orgid (:orgid resolvedmsg)
                   ;tag (:tag (:h resolvedmsg))
                   listuuids (map convertUUID suuids)
                   pgid (:parkinggroupid resolvedmsg)
                   pzid (:uuid (:h resolvedmsg))
                   ts (:t (:h resolvedmsg))
                   time (c/from-long (quot ts 1000))
                   pzcont1 (actlogger/get_one_parkingzone siteid (convertUUID pzid))
                   pzcont (if (> (count pzcont1) 0) (:config (nth pzcont1 0)) "")
                   pzconf (if (> (count pzcont) 0) (:spots (json/read-str pzcont :key-fn keyword)) "")
                   existinguuids (if (> (count pzconf) 0) (map :uuid pzconf) [])

                   ;; If emptyspots is not provided, compute by
                   ;; subtracting the provided `spotuuids` from the
                   ;; values existing in the config (stored in
                   ;; Cassandra).
                   emptyspots (seq (cset/difference (set existinguuids) (set listuuids)))
                   maxempty (count emptyspots)
                   rmsg {
                         :spots (resolve-spots spots)
                         :type name
                         :orgid orgid
                         :siteid siteid
                         ;:configured_date time
                         ;:tag tag
                         ;:roi (resolve-roi roi)
                         :channel (:ch (:h resolvedmsg))
                         :nodeid nodeid
                         :parkingzoneid (convertUUID pzid)
                         :time time
                         }

                   ]
                (actlogger/log_parking_zone {:orgid orgid :siteid siteid :type "Demarcated" :parkinggroupid pgid :parkingzoneid (convertUUID pzid) :state (json/write-str rmsg :value-fn time-aware-value-writer)})
                ;; Refrain from clearing Cassandra table, as we will
                ;; write *both* empty and non-empty spots.
                ;(actlogger/cleanup_parking_zone (convertUUID pzid))
                (mqtt-publish-parking resolvedmsg spots "DemarcatedParkingEvent" clientid)
                (dotimes [i maxcnt]
                  (let[
                       psid (nth suuids i)
                       oid (nth (nth ouuids i) 0)
                       co (:co (nth (:o resolvedmsg) i))
                       ts (:s (nth (:o resolvedmsg) i))
                       rts (:t (:h resolvedmsg))
                       resolvedparking (merge (resolve-parking resolvedmsg psid oid ts) {:co co})
                       ] (do
                           (log-parking-current-status-value (merge resolvedparking {:rsince rts})))))
                (dotimes [i maxempty]
                  (let[
                       psid (nth emptyspots i)
                       oid ""
                       ts (:t (:h resolvedmsg))
                       resolvedparking (resolve-empty-parking resolvedmsg psid oid ts)
                       ] (do
                           (log-parking-current-status-value (merge resolvedparking {:rsince ts})))))))

            ("NewAllConfig" "AllConfig" "BatchType" "BatchConfig")
            (if (not= (:siteid resolvedmsg) "Unknown")
              (process-all-config resolvedmsg))

            "Inventory"
            (send-cfg-request-kafka resolvedmsg)

            "DemarcatedParkingConfig"
            (if (not= (:siteid resolvedmsg) "Unknown")
              (let[
                   pzid (:uuid (:h raw_msg))
                   spots (:spots (:roi raw_msg))
                   ;removed (:removed raw_msg)
                   removed (get-removed-spots (convertUUID pzid) spots)
                   maxremoved (count removed)
                   maxcnt (count spots)
                   ts (:t (:h raw_msg))
                   nodeid (:nodeid resolvedmsg)
                   siteid (:siteid resolvedmsg)
                   orgid (:orgid resolvedmsg)
                   tag (:tag (:h resolvedmsg))
                   roi (:roi resolvedmsg)
                   parkinggroupid (:parkinggroupid resolvedmsg)
                   parkingzoneid (:uuid (:h resolvedmsg))
                   time (c/from-long (quot ts 1000))
                   rmsg {
                         :spots (resolve-conf-spots spots)
                         :type name
                         :orgid orgid
                         :siteid siteid
                         :tag tag
                         :roi (resolve-roi roi)
                         :channel (:ch (:h raw_msg))
                         :description (:des (:h raw_msg))
                         :name (:n (:h raw_msg))
                         :tags (:tag (:h raw_msg))
                         :active (:e (:h raw_msg))
                         :nodeid nodeid
                         :parkingzoneid (convertUUID parkingzoneid)
                         :configured_date time
                         }
                   ]
                (spyf :debug "DemarcatedParkingConfig: %s" resolvedmsg)
                ;(spy :error rmsg)
                (actlogger/log_parking_zone {:orgid orgid :siteid siteid :type "Demarcated" :parkinggroupid parkinggroupid :parkingzoneid (convertUUID parkingzoneid) :config (json/write-str rmsg :value-fn time-aware-value-writer)})
                (mqtt-publish-parking resolvedmsg spots "DemarcatedParkingConfig" clientid)
                (dotimes [i maxcnt]
                  (let[
                       psid (:uuid (nth spots i))
                       spot (nth spots i)
                       active (:e spot)
                       world (:world spot)
                       latarray (:lat world)
                       latlen (count latarray)
                       lonarray (:lon world)
                       lonlen (count lonarray)
                       v (:v (:img spot))
                       u (:u (:img spot))
                       vlen (count v)
                       ulen (count u)
                       lat1 (if (> latlen 0) (nth latarray 0) 0)
                       lat2 (if (> latlen 1) (nth latarray 1) 0)
                       lat3 (if (> latlen 2) (nth latarray 2) 0)
                       lat4 (if (> latlen 3) (nth latarray 3) 0)
                       lng1 (if (> lonlen 0) (nth lonarray 0) 0)
                       lng2 (if (> lonlen 1) (nth lonarray 1) 0)
                       lng3 (if (> lonlen 2) (nth lonarray 2) 0)
                       lng4 (if (> lonlen 3) (nth lonarray 3) 0)
                       x1 (if (> vlen 0) (nth v 0) 0)
                       x2 (if (> vlen 1) (nth v 1) 0)
                       x3 (if (> vlen 2) (nth v 2) 0)
                       x4 (if (> vlen 3) (nth v 3) 0)
                       y1 (if (> ulen 0) (nth u 0) 0)
                       y2 (if (> ulen 1) (nth u 1) 0)
                       y3 (if (> ulen 2) (nth u 2) 0)
                       y4 (if (> ulen 3) (nth u 3) 0)
                       resolvedparking (resolve-parking-config resolvedmsg psid pzid active x1 x2 x3 x4 y1 y2 y3 y4 lat1 lat2 lat3 lat4 lng1 lng2 lng3 lng4)
                       ] (do
                           (log-parking-current-status-value resolvedparking))))
                (dotimes [j maxremoved]
                  (actlogger/log_parking_spot_removed (nth removed j)))))

            "NonDemarcatedParkingEvent"
            (if (not= (:siteid resolvedmsg) "Unknown")
              (let[
                   pzid (:uuid (:h resolvedmsg))
                   spots (:o raw_msg)
                   siteid (:siteid resolvedmsg)
                   orgid (:orgid resolvedmsg)
                   pgid (:parkinggroupid resolvedmsg)
                   ts (:t (:h resolvedmsg))
                   ;tag (:tag (:h resolvedmsg))
                   channel (:ch (:h raw_msg))
                   ;roi (:roi resolvedmsg)
                   time (c/from-long (quot ts 1000))
                   pzcont1 (actlogger/get_one_parkingzone siteid (convertUUID pzid))
                   pzcont (if (> (count pzcont1) 0) (nth pzcont1 0) "")
                   existinguuids (if (nil? (:state pzcont)) [] (map :uuid (:spots (json/read-str (:state pzcont) :key-fn keyword))))
                   suuids (map convertUUID (map :uuid spots))
                   emptyspots (seq (cset/difference (set existinguuids) (set suuids)))
                   rmsg {
                         :spots (resolve-nondem-spots spots)
                         :type name
                         :orgid orgid
                         :siteid siteid
                         ;:tag tag
                         ;:roi (resolve-roi roi)
                         :channel channel
                         :nodeid nodeid
                         :parkingzoneid (convertUUID pzid)
                         :time time
                         }
                   ]
                (actlogger/log_parking_zone {:orgid orgid :siteid siteid :type "NonDemarcated" :parkinggroupid parkinggroupid :parkingzoneid (convertUUID pzid) :state (json/write-str rmsg :value-fn time-aware-value-writer)})
                ;; Clear stale entries from Cassandra before we write
                ;; each spot entry.
                (actlogger/cleanup_parking_zone (convertUUID pzid))
                (mqtt-publish-parking resolvedmsg spots "NonDemarcatedParkingEvent" clientid)
                (dotimes [i (count emptyspots)]
                 (let[
                      oid (nth emptyspots i)
                      psid oid
                      occupancy false
                      resolvedparking { :parkingzoneid (convertUUID pzid) :parkingspotid psid :occupancy false :nodeid nodeid :since ts :rsince ts :channel channel
 :demarcated false :siteid siteid :orgid orgid :objectid oid :parkinggroupid pgid }
                      ] (do
                          (spy :debug resolvedparking)
                          (log-parking-historic (merge resolvedparking {:rsince ts})))))
                (dotimes [i (count spots)]
                  (let[
                       ts (:s (nth (:o raw_msg) i))
                       co (:co (nth (:o raw_msg) i))
                       rts (:t (:h raw_msg))
                       oid (:uuid (nth spots i))
                       psid oid
                       spot (nth spots i)
                       world (:world spot)
                       latarray (:lat world)
                       latlen (count latarray)
                       lonarray (:lon world)
                       lonlen (count lonarray)
                       v (:v (:img spot))
                       u (:u (:img spot))
                       vlen (count v)
                       ulen (count u)
                       lat1 (if (> latlen 0) (nth latarray 0) 0)
                       lng1 (if (> lonlen 0) (nth lonarray 0) 0)
                       lat2 (if (> latlen 1) (nth latarray 1) 0)
                       lng2 (if (> lonlen 1) (nth lonarray 1) 0)
                       lat3 (if (> latlen 2) (nth latarray 2) 0)
                       lng3 (if (> lonlen 2) (nth lonarray 2) 0)
                       lat4 (if (> latlen 3) (nth latarray 3) 0)
                       lng4 (if (> lonlen 3) (nth lonarray 3) 0)
                       x1 (if (> vlen 0) (nth v 0) 0)
                       x2 (if (> vlen 1) (nth v 1) 0)
                       y1 (if (> ulen 0) (nth u 0) 0)
                       y2 (if (> ulen 1) (nth u 1) 0)
                       resolvedparking (merge (resolve-nondemarcated-parking resolvedmsg psid pzid ts x1 x2 y1 y2 lat1 lng1 lat2 lng2 lat3 lng3 lat4 lng4 pgid oid true) {:co co})
                       ] (do
                           (log-parking-current-status-value (merge resolvedparking {:rsince rts})))))))

            "NonDemarcatedParkingConfig"
            (if (not= (:siteid resolvedmsg) "Unknown")
              (let [
                    nodeid (:nodeid resolvedmsg)
                    siteid (:siteid resolvedmsg)
                    orgid (:orgid resolvedmsg)
                    parkinggroupid (:parkinggroupid resolvedmsg)
                    parkingzoneid (:uuid (:h resolvedmsg))
                    ts (:t (:h resolvedmsg))
                    tag (:tag (:h resolvedmsg))
                    roi (:roi resolvedmsg)
                    time (c/from-long (quot ts 1000))
                    rmsg {
                          :type name
                          :orgid orgid
                          :siteid siteid
                          :tag tag
                          :roi (resolve-roi roi)
                          :channel (:ch (:h resolvedmsg))
                          :active (:e (:h resolvedmsg))
                          :nodeid nodeid
                          :parkingzoneid (convertUUID parkingzoneid)
                          :configured_date time
                          }
                ]
                (actlogger/log_parking_zone {:orgid orgid :siteid siteid :type "NonDemarcated" :parkinggroupid parkinggroupid :parkingzoneid (convertUUID parkingzoneid) :config (json/write-str rmsg :value-fn time-aware-value-writer)})
                (mqtt-publish-parking resolvedmsg "" "NonDemarcatedParkingConfig" clientid)))

            ("LineCrossingConfig" "ObjectEnteringConfig" "ObjectLeavingConfig" "ObjectDwellConfig")
            (if (not= (:siteid resolvedmsg) "Unknown")
              (log-and-publish-traffic-config raw_msg resolvednode clientid))

            ;; Traffic detection events
            ("LineCrossingEvent" "ObjectEnteringEvent" "ObjectLeavingEvent" "ObjectDwellEvent")
            (if (not= (:siteid resolvedmsg) "Unknown")
              (log-and-publish-traffic-event raw_msg resolvednode clientid))

            "DeviceAlarm"
            (do
              (spyf :info "Got alarm: %s" resolvedmsg)
              (let [alarmType (:alarmType resolvedmsg)
                    alarmName (:name resolvedmsg)
                    alarmSeverity (:alarmSeverity resolvedmsg)
                    alerts (device-alarm resolvedmsg)
                    almsg (merge resolvedmsg
                                 {:sitename (:sitename resolvednode)
                                  :nodename (:nodename resolvednode)
                                  :orgname (:orgname resolvednode)
                                  :siteaddress (:siteaddress resolvednode)
                                  :category (get-alarm-category alarmType)
                                  :bssid (:bssid resolvednode)
                                  :nodehw (:nodehw resolvednode)})]
                (dorun (map (fn [alert]
                              (let [alertid (:alertid alert)
                                    resolvedalarm (spyf :debug "resolvedalert: %s" (assoc almsg :alertid alertid :type alarmType :severity alarmSeverity :name alarmName))
                                    alarmmsg (dissoc resolvedalarm :alarmType :alarmSeverity)
                                    jsonalert (spyf :debug "jsonalert: %s" (json/write-str alarmmsg))]
                                (mqtt-publish topic jsonalert clientid)
                                )) alerts))))

            "GpsSample"
                (do
                  (update-node-gps resolvedmsg)
                  ;(mqtt-publish topic jsonmsg clientid)
                  )

            (if (not= (:siteid resolvedmsg) "Unknown")
              (do
                (spyf :debug "Default: %s" resolvedmsg)
                (process-all-config resolvedmsg))
              (spyf :debug "Ignoring: %s" raw_msg)))))
      (catch Exception e
        (error (format "DeviceServiceWorker Exception %s for msg %s" e (if (bytes? mpmsg) (keywordize-keys (msgpk/unpack mpmsg)) mpmsg)))
        (error e)))))

(defn ack-timed-process-device-msg
  [chan {:keys [delivery-tag] :as meta} payload]
  (process-device-msg payload)
  (lb/ack chan delivery-tag))

(def msg-pack-response (msgpk/pack {"response" "OK"}))

(defn query-exec-msgpk [mpmsg]
  ;(spyf :debug "Sending response back to device service %s" msg)
  (counters/inc! (metric-factory :counter
                                 ["messages-queued-for-processing"]))
  (meters/mark! (metric-factory :meter
                                ["zmq-dequeue"]))
  (.execute thread-pool
            (fn []
              (counters/dec! (metric-factory :counter
                                             ["messages-queued-for-processing"]))
              (meters/mark! (metric-factory :meter
                                            ["process-message"]))
              (process-device-msg mpmsg)))
  msg-pack-response)

(defn startworker []
  (let [port (:fe (:mp (:devsvc zmqconf)))
        dealersocket (format "tcp://*:%s" port)]
    (info "Device Service.Worker started....")
    (worker/startasyncpullworker dealersocket "msgpack" query-exec-msgpk)))


(defn rmq-consumer-callback
  [chan
   {:keys [delivery-tag]
    :as meta}
   ^bytes payload]
  (try
    (counters/inc! (metric-factory :counter
                                   ["rmq-messages-queued-for-processing"]))
    (meters/mark! (metric-factory :meter
                                  ["rmq-dequeue"]))
    (process-device-msg payload)
    (lb/ack chan delivery-tag)
    (catch Throwable e
      (error e))
    (finally
      (counters/dec! (metric-factory :counter
                                     ["rmq-messages-queued-for-processing"]))
      (meters/mark! (metric-factory :meter
                                    ["process-rmq-message"])))))

(defrecord DeviceServiceWorker [config
                                channels
                                amqp-connection
                                metrics-registry]
  component/Lifecycle
  (start [component]
    (info "Starting Device Service Worker.")
    (let [{:keys [registry
                  prefix]} metrics-registry]
      (alter-var-root #'metric-factory
                      (fn [& _]
                        (dealer.metrics/metric-factory-factory "dealer.devsvcworker"
                                                               registry
                                                               prefix)))
      (alter-var-root #'utils.async.async_chans/metric-factory
                      (fn [& _]
                        (dealer.metrics/metric-factory-factory "utils.async.async_chans"
                                                               registry
                                                               prefix))))
    ;; As per https://www.rabbitmq.com/api-guide.html, we want a
    ;; consumer per connection. We also want to provide backpressure
    ;; to our MQ.
    (let [{:keys [consumer-count
                  prefetch
                  queue-name
                  exchange]} config
          {:keys [connection]} (spy :debug amqp-connection)
          channels (atom [])]
      (dotimes [_ consumer-count]
        (let [chan (->> connection
                        (spy :debug)
                        lch/open
                        (spy :debug))
              {:keys [queue]} (spy :debug (lq/declare chan
                                                      queue-name
                                                      {:exclusive false
                                                       :auto-delete false
                                                       :durable true}))]
          ;; Cribbed from legacy's sys.config
          (spy :debug (le/declare chan
                                  exchange
                                  "topic"
                                  {:exclusive false
                                   :auto-delete false
                                   :durable true}))
          ;; Unbinding unused exchanges from 3.0.5 Release (Split Brain)
          (lq/unbind chan queue exchange "#")
          (lq/unbind chan queue exchange "*.gps.*")

          (lq/bind chan queue exchange {:routing-key "*.login.*"})
          (lb/qos chan prefetch)
          (lc/subscribe chan
                        queue
                        rmq-consumer-callback
                        {})
          (swap! channels conj chan)))
      (startworker)
      (info "Started Device Service Worker.")
      (assoc component
             :channels @channels)))
  (stop [component]
    (info "Stopping Device Service Worker.")
    (doseq [channel channels]
      (amqp/close-channel channel))
    (info "Stopped.")
    component))

(defn new-device-service-worker
  [config]
  (map->DeviceServiceWorker {:config config}))
