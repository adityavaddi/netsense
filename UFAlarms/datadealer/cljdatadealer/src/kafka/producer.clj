(ns kafka.producer
  (:require [clojure.tools.logging :refer :all]
            [clojure.string :as string]
            [com.stuartsierra.component :as component])
  (:import [org.apache.kafka.clients.producer KafkaProducer ProducerRecord RecordMetadata]
           [java.util Properties]))

(defonce schedule-topic (atom "ms.request.schedule"))
(defonce trigger-topic (atom "ms.trigger.schedule"))

; TODO: Some of these should be in config file
(defn producer-connector
  [config]
  (let [brokers (string/join "," (map #(str % ":" (:port config)) (:host config)))
        st (:schedule-topic config)
        tt (:trigger-topic config)
        props (Properties.)]
    (if st (reset! schedule-topic st))
    (if tt (reset! trigger-topic tt))
    (doto props
      (.put "bootstrap.servers" brokers)
      (.put "value.serializer" org.apache.kafka.common.serialization.StringSerializer)
      (.put "key.serializer" org.apache.kafka.common.serialization.StringSerializer)
      (.put "acks" "all")
      (.put "buffer.memory" 33554432)
      (.put "compression.type" "none")
      (.put "retries" "3")
      (.put "client.id" "datadealer-producer")
      )
    (KafkaProducer. props)))

(defonce producer (atom nil))
(defonce alarm-topic (atom "corenode.alarm"))
(defonce gps-topic (atom "ms.request.gps"))

(defn send-to-producer
  "Send a string message to Kafka"
  [topic message]
  (let [data (ProducerRecord. topic message)]
    (.send @producer data)))

(defn send-to-producer-kv
  "Send a key, value pair to Kafka"
  [topic k v]
  (let [data (ProducerRecord. topic k v)]
    (.send @producer data)))

(defn kafka-alert [message]
  (send-to-producer @alarm-topic message))

(defn connect
  [config]
  (loop [delay 250]
    (let [connection-attempt (try
                               (spy :debug (producer-connector config))
                               (catch java.net.ConnectException e
                                 :connect-exception)
                               (catch java.io.IOException e
                                 :io-exception))]
      (cond
        (instance? KafkaProducer connection-attempt)
        (spyf :info
          "Connected to Kafka producer: %s"
          connection-attempt)
        (= connection-attempt
          :connect-exception) (do
                                (infof "Could not connect. Sleeping %d, then retrying."
                                  delay)
                                (Thread/sleep delay)
                                (recur (* 2 delay)))
        (= connection-attempt
          :io-exception) (do
                           (infof "Could not connect. Sleeping %d, then retrying."
                             delay)
                           (Thread/sleep delay)
                           (recur (* 2 delay)))
        :else (throw (ex-info "Could not connect to Kafka producer"
                       {:connection connection-attempt}))))))

(defrecord KafkaConnector [config
                          connection]
  component/Lifecycle
  (start [component]
    (infof "Connecting to %s Kafka producer."
      config)
    (when (:alarm-topic config)
      (reset! alarm-topic (:alarm-topic config)))
    (when (:gps-topic config)
      (reset! gps-topic (:gps-topic config)))
    (let [config (select-keys config
                   [:host
                    :trigger-topic
                    :schedule-topic
                    :port])
          connection (spy :debug (connect config))]
      (reset! producer connection)
      (-> component
        (assoc :connection
          connection))))
  (stop [component]
    (info "Terminating Kafka producer connection.")
    (.close connection)
    (info "Terminated.")))

(defn new-kafka-producer
  [config]
  (map->KafkaConnector {:config config}))
