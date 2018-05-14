(ns kafka.consumer
  (:require [clojure.tools.logging :refer :all]
            [clojure.string :as string]
            [com.stuartsierra.component :as component])
  (:import [org.apache.kafka.clients.consumer KafkaConsumer]
           [java.util Properties]))

(defonce device-topic-out (atom "node.command"))
(defonce device-topic-event (atom "va.node.evt"))
(defonce device-topic-config (atom "va.node.cfg"))

(defn consumer-connector
  [config topics group]
  (let [brokers (string/join "," (map #(str % ":" (:port config)) (:host config)))
        props (Properties.)]
    (doto props
      (.put "bootstrap.servers" brokers)
      (.put "key.deserializer" "org.apache.kafka.common.serialization.ByteArrayDeserializer")
      (.put "value.deserializer" "org.apache.kafka.common.serialization.ByteArrayDeserializer")
      (.put "enable.auto.commit" "false")
      (.put "session.timeout.ms" "10000")
      (.put "group.id", (:group config))
      (.put "max.poll.records" "10")  ; We have 40 threads to work on this, so don't take bites too big, or we can get error committing  offset during poll calls.  Default is 500.
      (.put "client.id" (str "consumer-" (:suffix config (java.util.UUID/randomUUID)))))
    (let [consumer (->
                    props
                    (KafkaConsumer.))]
     (.subscribe consumer (vec topics))
      consumer)))

(defn connect
  [config topics group]
  (loop [delay 250]
    (let [connection-attempt (try
                               (consumer-connector config topics group)
                               (catch java.net.ConnectException e
                                 :connect-exception)
                               (catch java.io.IOException e
                                 :io-exception))]
      (cond
        (instance? KafkaConsumer connection-attempt) (spyf :info "Connected to Kafka consumer: %s"
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
        :else (throw (ex-info "Could not connect to Kafka consumer"
                       {:connection connection-attempt}))))))

(defrecord KafkaConnector [config
                           connection]
  component/Lifecycle
  (start [component]
    (infof "Connecting to %s Kafka consumer."
      config)
    (let [config (select-keys config
                   [:host
                    :topic
                    :device-topic-out
                    :device-topic-event
                    :device-topic-config
                    :port
                    :suffix
                    :va-group
                    :group])
          dto (:device-topic-out config)
          dte (:device-topic-event config)
          dtc (:device-topic-config config)]
      (when dte
        (reset! device-topic-event dte))
      (when dtc
        (reset! device-topic-config dtc))
      (when dto
        (reset! device-topic-out dto))
      (-> component
        (assoc :connection
          (connect config [@device-topic-event @device-topic-config] (:va-group config))))))
  (stop [component]
    (info "Terminating Kafka consumer connection.")
    (.close connection)
    (info "Terminated.")))

(defn new-kafka-consumer
  [config]
  (map->KafkaConnector {:config config}))

(defrecord KafkaSensorConnector [config
                           connection]
  component/Lifecycle
  (start [component]
    (infof "Connecting to %s Sensor Kafka consumer."
           config)
    (let [config (select-keys config
                              [:host
                               :topic
                               :trigger-lux-topic
                               :presence-topic
                               :driver-level-topic
                               :ambient-light-topic
                               :port
                               :suffix
                               :va-group
                               :group])
          tlt (:trigger-lux-topic config)
          pt (:presence-topic config)
          dlt (:driver-level-topic config)
          alt (:ambient-light-topic config)]
      (-> component
          (assoc :connection
                 (connect config [(or tlt "trigger_lux") (or pt "presence") (or dlt "driver_level") (or alt "ambient_light")] (:group config))))))
  (stop [component]
    (info "Terminating Sensor Kafka consumer connection.")
    (.close connection)
    (info "Terminated.")))

(defn new-kafka-sensor-consumer
  [config]
  (map->KafkaSensorConnector {:config config}))
