(ns mqtt.core_lib (:gen-class)
  (:use clojure.tools.logging)
  (:import (java.nio.charset Charset))
  (:import (org.eclipse.paho.client.mqttv3 MqttCallback))
  (:import (org.eclipse.paho.client.mqttv3 MqttClient))
  (:import (org.eclipse.paho.client.mqttv3 MqttConnectOptions))
  (:import (org.eclipse.paho.client.mqttv3 MqttDeliveryToken))
  (:import (org.eclipse.paho.client.mqttv3 MqttException))
  (:import (org.eclipse.paho.client.mqttv3 MqttMessage))
  (:import (org.eclipse.paho.client.mqttv3 MqttTopic)))

(defn create-message-from-bytes [bytes & {:keys [qos ret]}]
  (let [message (MqttMessage. bytes)]
    (doto message
      (.setQos  (if (nil? qos) 0 qos))
      (.setRetained (if (nil? ret) false ret)))))

(defn create-message [message-string & {:keys [qos ret]}]
  (create-message-from-bytes (.getBytes message-string (Charset/forName "UTF-8")) qos ret))

(defn create-client
      [broker-uri client-id]
      (let
        [connect-options (MqttConnectOptions.)
         client (MqttClient. broker-uri client-id)]
        (doto connect-options
              (.setCleanSession 'true)
              ;(.setMaxInflight 1000)
              (.setKeepAliveInterval 30))
        (.connect client connect-options)
        client))

(defn connect [client]
  (if-not (.isConnected client)
    (if-let [connected (let [connect-options (MqttConnectOptions.)]
                         (doto connect-options
                           (.setCleanSession 'true)
                           (.setKeepAliveInterval 30))
                         (try
                           (.connect client connect-options)
                           true
                           (catch Exception e
                             false)))]
      connected
      (do
        (errorf "Connection to MQTT lost, reestablishing connection...")
        (Thread/sleep 5000)
        (recur client)))))

(defn get-topic
      [client topic-name ]
      (.getTopic client topic-name))

(defn publish
       [topic message]
       (.waitForCompletion (.publish topic message)))

(defn connection-lost
  [cause]
  (debugf "[MQTT] connection lost %s" cause))

(defn delivery-complete
  [token]
  (debugf "[MQTT] delivery complete %s" (.. token getMessage toString)))

(defn subscribe
  [client topic-name callback]
  (let [callbacks (reify
                   MqttCallback
                   (deliveryComplete [this t] (delivery-complete t))
                   (connectionLost [this c] (connection-lost c))
                   (messageArrived [this t m] (callback t m)))]
    (doto client
      (.setCallback callbacks)
      (.subscribe topic-name))))

