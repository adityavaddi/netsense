(ns ^{:doc "The Device Service Controller."}
    dealer.devsvcctrl
  (:gen-class)
  (:require [amqp.core :as amqp]
            [clj-time
             [coerce :as c]
             [core :as time-core]
             [format :as f]]
            [clojure.data.json :as json]
            [clojure.tools.logging :refer :all]
            [clojure.walk :refer :all]
            [com.stuartsierra.component :as component]
            [dealer.metrics]
            [langohr
             [basic :as lb]
             [channel :as lch]
             [exchange :as le]]
            [logging.activitylogger :as al]
            [metrics.meters :as meters]
            [metrics.timers :as timers :refer [start stop]]
            [mqtt.core :refer :all]
            [msgpack.core :as msgpk]
            [utils.config :as conf])
  (:import com.sensity.netsense.zmq.AsyncSend))

(defonce metric-factory
  (dealer.metrics/metric-factory-stub "dealer.devsvcctrl"))

(def devsvcctrl-timer-name "device-service-control")

(def async-connection-failure-meter-name "async-connection-failure")
(defonce async-connection-failure-meter
  (metric-factory :meter
                  [async-connection-failure-meter-name]))

(def zmqconf (conf/zmqservice))
(def mqtt-topic "DeviceControl")

(declare query-exec-msgpk)

(defn mqtt-send-message
  [orgid siteid nodeids topic message-map]
  (let [clientid (format "data-dealer-%s-%s"
                         (Thread/currentThread)
                         (str (java.util.UUID/randomUUID)))
        jsonmsg (json/write-str (stringify-keys message-map))]
    (debugf "Publishing message to MQTT %s" jsonmsg)
    (doseq [nodeid nodeids]
      (let [topic (format "/streamv1/%s/%s/%s/%s" orgid siteid nodeid topic)]
        (mqtt-publish topic jsonmsg clientid)))))

(defonce active-lfs
  (atom {}))

(def pending-override-resets
  "A map containing pending futures for setting a node back to schedule."
  (atom {}))

(defn cancel-override-reset
  [nodeid]
  (when-let [f (get @pending-override-resets nodeid)]
    (debugf "Canceling LightSetAuto for node %s" nodeid)
    (future-cancel f)))

(defn register-override-reset
  [timeout nodeid]
  (->> (future
         (let [;; Convert seconds to milliseconds
               timeout (* timeout 1e3)]
           (debugf "Overriding LightForceState for node %s in %s milliseconds" nodeid (str timeout))
           (Thread/sleep timeout)
           (query-exec-msgpk {:nodeprops {:type "LightingSetAuto" :nodeid [nodeid]}})
           (swap! pending-override-resets dissoc nodeid)))
       (swap! pending-override-resets assoc nodeid)))

(defn lighting-force-state
  [{{:keys [orgid]} :orgprops
    {:keys [siteid]} :siteprops
    {:keys [nodeid
            level
            timeout
            harvesting]
     nametype :type
     priority :pri
     :or {harvesting false
          priority 3}} :nodeprops
    :as jsmap}]
  {:pre [(seq nodeid)
         level
         (= "LightingForceState"
            nametype)
         (integer? priority)]}
  (let [{nodes-with-existing-override true
         nodes-without-existing-override false} (group-by (fn [nodeid]
                                                            (< (get @active-lfs nodeid 256)
                                                               priority))
                                                          nodeid)
        startdt (if (pos? timeout) (.toDate (time-core/plus (time-core/now) (time-core/seconds timeout))) nil)
        command {:nodeid nodes-without-existing-override
                 :name nametype
                 :pri priority
                 :mask 1
                 :level level
                 :qualifiers "undefined"
                 :ftype "Volatile"}
        log_map {:nodeid nodes-without-existing-override
                 :isscheduled false
                 :harvest_trigger harvesting
                 :policy "override"
                 :priority priority
                 :driver level
                 :startdt startdt}]
    (when (seq nodes-with-existing-override)
      (debugf "Not sending LFS as higher priority LFS has already been sent to following nodes: %s"
             (vec nodes-with-existing-override)))
    (if-not (seq nodes-without-existing-override)
      (debugf "No nodes to send LFS to")
      ;; Only proceed if the node is assigned to a Site
      (when (complement (or (nil? orgid) (nil? siteid) (= orgid "Unknown") (= siteid "Unknown")))
        (doseq [n nodes-without-existing-override]
          (swap! active-lfs
                 assoc n priority))

        (debugf "Sending %s message %s" nametype command)
        ;(mqtt-send-message orgid siteid nodes-without-existing-override mqtt-topic command)
        (al/log_light_mode log_map)
        (when (pos? timeout)
          (->> nodes-without-existing-override
               (map cancel-override-reset)
               dorun)
          (->> nodes-without-existing-override
               (map #(register-override-reset timeout %))
               dorun))
        (stringify-keys command)))))

(defn prepare-devsvc-command [jsmap]
  (let [nametype (get-in jsmap [:nodeprops :type])
        nodeid (get-in jsmap [:nodeprops :nodeid])
        timeout (get-in jsmap [:nodeprops :timeout])
        priority (get-in jsmap [:nodeprops :pri] 3)
        orgid (get-in jsmap [:orgprops :orgid] "Unknown")
        siteid (get-in jsmap [:siteprops :siteid] "Unknown")]
    (if-not (seq nodeid)
      (warnf "No nodes specified -- Not sending %s" jsmap)
      (case nametype

        "SensorSampleReq"
        (with-open [_ (timers/start (metric-factory :timer
                                                    [devsvcctrl-timer-name
                                                     nametype]))]
          (let [command {:nodeid nodeid
                         :name nametype
                         :sensor (get-in jsmap [:extprops :sensor])}]
            (debugf "Sending %s message %s" nametype command)
            ;(mqtt-send-message orgid siteid nodeid mqtt-topic command)
            (stringify-keys command)))

        "DeviceActionReq"
        (with-open [_ (timers/start (metric-factory :timer
                                                    [devsvcctrl-timer-name
                                                     nametype]))]
          (let [command {:nodeid nodeid
                         :name nametype
                         :action (get-in jsmap [:nodeprops :cmd])}]
            (debugf "Sending %s message %s" nametype command)
            ;(mqtt-send-message orgid siteid nodeid mqtt-topic command)
            (stringify-keys command)))

        "LightingForceState"
        (with-open [_ (timers/start (metric-factory :timer
                                                    [devsvcctrl-timer-name
                                                     nametype]))]
          (lighting-force-state jsmap))

        "LightingSetAuto"
        (with-open [_ (timers/start (metric-factory :timer
                                                    [devsvcctrl-timer-name
                                                     nametype]))]
          (let [command {:nodeid nodeid :name nametype}
                log_map {:nodeid nodeid
                         :isscheduled true
                         :policy "scheduled"
                         :harvest_trigger false}]
            (doseq [n nodeid]
              (swap! active-lfs
                     dissoc n))
            (debugf "Sending %s message %s" nametype command)
            ;(mqtt-send-message orgid siteid nodeid mqtt-topic command)
            (al/log_light_mode log_map)
            (stringify-keys command)))

        "ProximityDimming"
        (with-open [_ (timers/start (metric-factory :timer
                                                    [devsvcctrl-timer-name
                                                     nametype]))]
          (let [command {:nodeid nodeid
                         :name nametype
                         :pri 3
                         :mask 1
                         :minLevel (Integer/parseInt (get-in jsmap [:pdprofileprops :minLevel]))
                         :maxLevel (Integer/parseInt (get-in jsmap [:pdprofileprops :maxLevel]))
                         :beginTime (get-in jsmap [:pdprofileprops :beginTime])
                         :endTime (get-in jsmap [:pdprofileprops :endTime])
                         :radius (Integer/parseInt (get-in jsmap [:pdprofileprops :radius]))
                         :detection_duration (Integer/parseInt (get-in jsmap [:pdprofileprops :detection_duration]))
                         :qualifiers "undefined"
                         :ftype "Volatile"}]
            (debugf "Sending ProximityDimming message %s" command)
            ;(mqtt-send-message orgid siteid nodeid mqtt-topic command)
            (stringify-keys command)))


        "DaylightHarvesting"
        (with-open [_ (timers/start (metric-factory :timer
                                                    [devsvcctrl-timer-name
                                                     nametype]))]
          (let [command {:nodeid nodeid
                         :name nametype
                         :pri 3
                         :mask 1
                         :setPoint (Float/parseFloat (get-in jsmap [:dhprofileprops :setPoint]))
                         :gain (Float/parseFloat (get-in jsmap [:dhprofileprops :gain]))
                         :resetTime (Float/parseFloat (get-in jsmap [:dhprofileprops :resetTime]))
                         :minDrive (Integer/parseInt (get-in jsmap [:dhprofileprops :minDrive]))
                         :maxDrive (Integer/parseInt (get-in jsmap [:dhprofileprops :maxDrive]))
                         :slewRate (Float/parseFloat (get-in jsmap [:dhprofileprops :slewRate]))
                         :idleInterval (Integer/parseInt (get-in jsmap [:dhprofileprops :idleInterval]))
                         :pollInterval (Integer/parseInt (get-in jsmap [:dhprofileprops :pollInterval]))
                         :qualifiers "undefined"
                         :ftype "Volatile"}
                log_map  {:nodeid nodeid
                          :harvest_trigger true}]
            (debugf "Sending DaylightHarvesting message %s" command)
            ;(mqtt-send-message orgid siteid nodeid mqtt-topic command)
            (stringify-keys command)))

        "AssignFirmware"
        (with-open [_ (timers/start (metric-factory :timer
                                                    [devsvcctrl-timer-name
                                                     nametype]))]
          (let [firmwareid (get-in jsmap [:firmwareprops :firmwareid])
                description (get-in jsmap [:nodeprops :description])
                target_type (get-in jsmap [:nodeprops :target_type])
                target_id (get-in jsmap [:nodeprops :target_id])
                model (get-in jsmap [:nodeprops :model])
                jobid (str (java.util.UUID/randomUUID))
                job {:nodeid nodeid
                     :name nametype
                     :jobid jobid
                     :model (.substring model (count "unode-"))
                     :firmwareid firmwareid}
                topic (format "/streamv1/%s/%s/%s/%s" orgid siteid jobid "OTAStatus")
                clientid (format "data-dealer-%s-%s"
                                 (Thread/currentThread)
                                 (str (java.util.UUID/randomUUID)))]
            (debugf "Sending %s message %s" nametype job)
            ;(mqtt-publish topic (json/write-str job) clientid)
            (al/log_ota {:firmwareid firmwareid
                         :jobid jobid
                         :status "JOB_SENT"
                         :success true
                         :description description
                         :orgid orgid
                         :siteid siteid
                         :target_type target_type
                         :target_id target_id
                         :model model
                         :node_count (count nodeid)})
            (al/log_otaids {:jobid jobid
                            :orgid orgid
                            :siteid siteid
                            :target_type target_type
                            :target_id target_id
                            :model model
                            :node_count (count nodeid)})
            (stringify-keys job)))

        "OTAStop"
        (with-open [_ (timers/start (metric-factory :timer
                                                    [devsvcctrl-timer-name
                                                     nametype]))]
          (let [jobid (get-in jsmap [:otaprops :jobid])
                {:keys [siteid orgid target_type target_id]}
                (into {} (al/get_org_site_from_ota_log jobid))]
            (al/log_ota {:jobid jobid
                         :status "JOB_STOP"
                         :success true
                         :orgid orgid
                         :siteid siteid
                         :target_type target_type
                         :target_id target_id})
            (stringify-keys {:jobid jobid :name nametype})))

        "OTAFaster"
        (with-open [_ (timers/start (metric-factory :timer
                                                    [devsvcctrl-timer-name
                                                     nametype]))]
          (let [jobid (get-in jsmap [:otaprops :jobid])
                {:keys [siteid orgid target_type target_id]}
                (into {} (al/get_org_site_from_ota_log jobid))]
            (al/log_ota {:jobid jobid
                         :status "JOB_FASTER"
                         :success true
                         :orgid orgid
                         :siteid siteid
                         :target_type target_type
                         :target_id target_id})
            (stringify-keys {:jobid jobid :name nametype})))

        "OTASlower"
        (with-open [_ (timers/start (metric-factory :timer
                                                    [devsvcctrl-timer-name
                                                     nametype]))]
          (let [jobid (get-in jsmap [:otaprops :jobid])
                {:keys [siteid orgid target_type target_id]}
                (into {} (al/get_org_site_from_ota_log jobid))]
            (al/log_ota {:jobid jobid
                         :status "JOB_SLOWER"
                         :success true
                         :orgid orgid
                         :siteid siteid
                         :target_type target_type
                         :target_id target_id})
            (stringify-keys {:jobid jobid :name nametype})))

        "ConfigResp"
        (with-open [_ (timers/start (metric-factory :timer
                                                    [devsvcctrl-timer-name
                                                     nametype]))]
          (let [kvpairs (get-in jsmap [:nodeprops :kvpairs])
                configtype (get-in jsmap [:nodeprops :configtype])
                command {:name nametype
                         :configtype configtype
                         :nodeid nodeid
                         :kvpairs kvpairs}]
            (debugf "Sending %s message %s" nametype command)
            ;(mqtt-send-message orgid siteid nodeid mqtt-topic command)
            (stringify-keys command)))

        "LightingScheduledEvent"
        (with-open [_ (timers/start (metric-factory :timer
                                                    [devsvcctrl-timer-name
                                                     nametype]))]
          (let [command {:nodeid nodeid
                         :name nametype
                         :schedules (:schedules jsmap)}
                log_map {:nodeid nodeid
                         :isscheduled true
                         :policy "scheduled"
                         :harvest_trigger false}]
            ;(spy :error jsmap)
            (debugf "Sending %s message %s" nametype command)
            ;(mqtt-send-message orgid siteid nodeid mqtt-topic command)
            (al/log_light_mode log_map)
            (stringify-keys command)))

        "LightingClearSchedule"
        (with-open [_ (timers/start (metric-factory :timer
                                                    [devsvcctrl-timer-name
                                                     nametype]))]
          (let [command {:nodeid nodeid :name nametype}]
            (debugf "Sending %s message %s" nametype command)
            ;(mqtt-send-message orgid siteid nodeid mqtt-topic command)
            (stringify-keys command)))

        (errorf "Received device control request %s" jsmap)))))

(def channel)

(defn sendtodevsvc
  [message]
  (with-open [_ (timers/start (metric-factory :timer
                                              [devsvcctrl-timer-name]))]
    (try
      (let [payload (msgpk/pack message)]
        (lb/publish channel "node.commands" "*" payload))
      (catch Exception e
        (doto e
          error
          throw)))))

(defn query-exec-msgpk [jsmap]
  (try
    (when-let [cmd (prepare-devsvc-command jsmap)]
      (sendtodevsvc cmd))
    (catch Exception e
      (doto e
        error
        throw))))

(defrecord DeviceServiceController [config
                                    channel
                                    amqp-connection
                                    metrics-registry]
  component/Lifecycle
  (start [component]
    (let [{:keys [registry
                  prefix]} metrics-registry]
      (alter-var-root #'metric-factory
                      (fn [& _]
                        (dealer.metrics/metric-factory-factory "dealer.devsvcctrl"
                                                               registry
                                                               prefix)))
      (alter-var-root #'async-connection-failure-meter
                      (fn [& _]
                        (metric-factory :meter
                                        [async-connection-failure-meter-name]))))
    (let [{:keys [exchange]} config
          {:keys [connection]} (spy :debug amqp-connection)
          chan (->> connection
                    (spy :debug)
                    lch/open
                    (spy :debug))]
      (le/declare chan
                  exchange
                  "topic"
                  {:exclusive false
                   :auto-delete false
                   :durable true})
      ;; TODO: consider having more than one channel (and thus, more
      ;; than one Thread) for dispatching commands to Nodes.
      (alter-var-root #'channel
                      (fn [_]
                        chan))
      (assoc component
             :channel chan)))
  (stop [component]
    (amqp/close-channel channel)
    (dissoc component
            :channel)))

(defn new-device-service-control
  [config]
  (map->DeviceServiceController {:config config}))
