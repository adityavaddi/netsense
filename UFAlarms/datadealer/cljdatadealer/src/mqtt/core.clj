(ns mqtt.core
  (:gen-class)
  (:require [clojure.tools.logging :refer :all]
            [com.stuartsierra.component :as component]
            [mqtt.core_lib :refer :all]
            [utils.config :as conf])
  (:import org.eclipse.paho.client.mqttv3.MqttException))

(def mqtt-broker (:host (conf/mqttservice)))
;(def client-id (:client-id conf/mqttservice))

(defn establish-connection
  []
  (let [client-id (format "Datadealer-%s-%s"
                          (Thread/currentThread)
                          (str (java.util.UUID/randomUUID)))
        client (create-client mqtt-broker client-id)]
    client))

(def client-connection (delay
            (info "Trying to connect to MQTT")
            (if-let [connected (try (establish-connection)
                                 (catch Exception e
                                   (error
                                    (spyf :error "MQTT connection failed: %s" e))
                                   false))]
              connected
              (do
                (Thread/sleep 5000)
                (recur)))))

(defn mqtt-publish [topic-name message-string client-id]
  (try
    (let [client @client-connection
          topic (get-topic client topic-name)
          message (create-message message-string)]
      (debugf "mqtt-publish(%s): %s" topic message)
      (publish topic message))
    (catch MqttException e
      (spyf :error "Caught MQTT exception %s" (.toString e))
      (connect @client-connection))))


; mqtt-subscribe usage:

;   (defn message-arrived
;     [topic mqtt-message]
;     (let [message (.toString mqtt-message)
;           map-msg (json/read-str message)]
;       (println (format "[%s] received: %s \n" topic message))
;       (println   map-msg)))

; (mqtt/mqtt-subscribe "/streamv1/#" "client-id" message-arrived)

(defn mqtt-subscribe
  [topic-name callback]
  (try
    (let [client @client-connection]
      (subscribe client topic-name callback))
    (catch MqttException e
      (spyf :error "Caught MQTT exception %s" (.toString e))
      (connect @client-connection))))

(defrecord MQTT []
  component/Lifecycle
  (start [component]
    (info "Starting MQTT.")
    @client-connection
    component)
  (stop [component]
    (info "Stopping MQTT.")
    ;; TODO: Clear `client-connection` and reset with new pending delay.
    component))

(defn new-mqtt
  []
  (map->MQTT {}))
