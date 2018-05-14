(ns ^{:doc "The MQTT listener."}
  dealer.mqttlistener
  (:gen-class)
  (:require [clojure
             [stacktrace :as trace]
             [walk :refer :all]]
            [clojure.data.json :as json]
            [clojure.tools.logging :refer :all]
            [com.stuartsierra.component :as component]
            [metrics.timers :as timers :refer [start stop]]
            [dealer.devsvcworker :as devsvcworker]
            [logging.activitylogger :as actlogger]
            [mqtt.core :refer :all]
            [msgpack.core :as msgpk]
            [utils
             [cape :as cape]
             [config :as conf]]))

;(defonce mqttlistener-timer (timers/timer "mqtt-listener"))

(def mqtt-topic "/streamv1/faileddelivery/legacy/#")

(defn failed-delivery
  [topic mqtt-message]
  (let [mqtt-map (msgpk/unpack (.getPayload mqtt-message))
        nodeid (get mqtt-map "nodeid")
        status (actlogger/get_latest_connection_status nodeid)
        packed-msg (msgpk/pack {"name" "ConnectionStatus" "nodeid" nodeid "status" "disconnected"})]
    (if (:isconnected status)
      (cape/cape-future devsvcworker/process-device-msg packed-msg))))

(defn startlistener []
  (mqtt-subscribe mqtt-topic failed-delivery))

(defrecord MqttListener []
  component/Lifecycle
  (start [component]
    (info "Starting MQTT listener...")
    (startlistener)
    component)
  (stop [component]
    (info "Stopping MQTT listener.")))

(defn new-mqtt-listener
  []
  (map->MqttListener {}))
