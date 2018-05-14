(ns kafka.streams
  (:require [clojure.tools.logging :refer :all]
            [clojure.string :as string]
            [com.stuartsierra.component :as component])
  (:import [org.apache.kafka.streams.processor WallclockTimestampExtractor]
           [org.apache.kafka.streams.processor.internals RecordQueue]
           [org.apache.kafka.streams KafkaStreams StreamsConfig]
           [org.apache.kafka.clients.consumer ConsumerConfig]
           [org.apache.kafka.streams.kstream KStreamBuilder ValueMapper]
           [org.apache.kafka.common.serialization Serdes]))

(defn streams-config
  [config]
  (let [brokers (string/join "," (map #(str % ":" (:port config)) (:host config)))
        props {StreamsConfig/APPLICATION_ID_CONFIG (str "acl-service-" (:suffix config (java.util.UUID/randomUUID)))
               StreamsConfig/BOOTSTRAP_SERVERS_CONFIG (spyf :debug "Brokers: %s" brokers)
               StreamsConfig/COMMIT_INTERVAL_MS_CONFIG 5000
               ConsumerConfig/AUTO_OFFSET_RESET_CONFIG "latest"
               StreamsConfig/KEY_SERDE_CLASS_CONFIG (.getName (.getClass (Serdes/String)))
               StreamsConfig/VALUE_SERDE_CLASS_CONFIG (.getName (.getClass (Serdes/String)))
               StreamsConfig/TIMESTAMP_EXTRACTOR_CLASS_CONFIG WallclockTimestampExtractor}]
    (StreamsConfig. props)))

(defn streams-builder
  []
  RecordQueue

  (KStreamBuilder.))

(defn start
  [builder config handler]
  (info "Starting Kafka stream...")
  (let [ks (KafkaStreams. builder config)
        connected (try
                    (.setUncaughtExceptionHandler ks handler)
                    (.start ks)
                    (error "Successfully started Kafka stream...")
                    true
                    (catch Exception e
                      (error (ex-info "Kafka stream connection failed: " e))
                      false))]
    (when (not connected)
      (errorf "Could not connect to Kafka steam. Sleeping %d, then retrying." 250)
      (Thread/sleep 250)
      (recur builder config handler))))

;(defn connect
;  [config]
;  (loop [delay 250]
;    (let [connection-attempt (try
;                               (streams-builder config)
;                               (catch java.net.ConnectException e
;                                 :connect-exception)
;                               (catch java.io.IOException e
;                                 :io-exception))]
;      (cond
;        (instance? KafkaStreams connection-attempt) (spyf :info "Connected to Kafka: %s"
;                                                           connection-attempt)
;        (= connection-attempt
;          :connect-exception) (do
;                                (infof "Could not connect. Sleeping %d, then retrying."
;                                  delay)
;                                (Thread/sleep delay)
;                                (recur (* 2 delay)))
;        (= connection-attempt
;          :io-exception) (do
;                           (infof "Could not connect. Sleeping %d, then retrying."
;                             delay)
;                           (Thread/sleep delay)
;                           (recur (* 2 delay)))
;        :else (throw (ex-info "Could not connect to Kafka"
;                       {:connection connection-attempt}))))))

(defrecord KafkaConnector [config
                           connection]
  component/Lifecycle
  (start [component]
    (infof "Connecting to %s Kafka."
      config)
;    (let [config (select-keys config
;                   [:host
;                    :port
;                    :group])]
;      (-> component
;        (assoc :connection
;          (connect config))))

    )
  (stop [component]
    (info "Terminating Kafka connection.")
    ;(.close connection)
    (info "Terminated.")))

(defn new-kafka-stream
  [config]
  (map->KafkaConnector {:config config}))
