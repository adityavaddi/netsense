(ns ^{:doc "The Config Service."}
    utils.config
  (:gen-class)
  (:require [biscuit.core :as digest]
            [clojure
             [edn :as edn]
             [set :as set]
             [walk :refer :all]]
            [clojure.data.json :as json]
            [clojure.java.io :as io :refer :all]
            [clojure.tools.logging :refer :all]
            [environ.core :as environ]))

(defn remove-empty-from-map
  [m]
  (->>
    m
    (remove (comp empty? second))
    (into {})))

(defn neo4jservice []
  (let [name (keyword (environ/env :name :development))
        resource (edn/read-string (slurp (io/resource "neo4j.edn")))
        environ-key-mapping {:neo4j-service :hosts}
        cli-args (set/rename-keys (select-keys environ/env
                                               (keys environ-key-mapping))
                                  environ-key-mapping)
        args (->
              cli-args
              (update
                 ;; To handle multiple hosts, take the string
                 ;; and split on `","`.
                 :hosts #(when %
                           (-> %
                           (.split ",")
                           seq)))
               (remove-empty-from-map))
        config (merge (name resource)
                      args)]
    (infof "Connecting to Neo4j using %s from environment %s" args config)
    config))

(defn zmqservice []
  (let [name (keyword (environ/env :name :development))
        resource (edn/read-string (slurp (io/resource "zmqserviceports.edn")))
        environ-key-mapping {:legacy-service :broker}
        cli-args (set/rename-keys (select-keys environ/env
                                               (keys environ-key-mapping))
                                  environ-key-mapping)
        config (update (name resource)
                       :devsvcctrl merge cli-args)]
    (infof "Connecting to ZMQ using %s" config)
    config))

(defn cassservice []
  (let [name (keyword (environ/env :name :development))
        resource (edn/read-string (slurp (io/resource "cassandra-conf.edn")))
        environ-key-mapping {:cassandra-service :host
                             :cassandra-keyspace :keyspace}
        cli-args (set/rename-keys (select-keys environ/env
                                               (keys environ-key-mapping))
                                  environ-key-mapping)
        args (->
               cli-args
               (update
                 ;; To handle multiple hosts, take the string
                 ;; and split on `","`.
                 :host #(when %
                           (-> %
                             (.split ",")
                             seq)))
               (remove-empty-from-map))
        config (merge (name resource)
                      args)]
    (infof "Connecting to Cassandra using %s from environment %s" args config)
    config))

(defn mqttservice []
  (let [name (keyword (environ/env :name :development))
        resource (edn/read-string (slurp (io/resource "mqtt-conf.edn")))
        mqtt-service (:mosquitto-service environ/env)
        mqtt-port (:mosquitto-port environ/env)
        client-id (:client-id environ/env)
        mqtt-host (if (and (not (empty? mqtt-service)) (not (empty? mqtt-port)))
                      (str "tcp://" mqtt-service ":" mqtt-port)
                      nil)
        cli-args (remove-empty-from-map {:host mqtt-host :client-id client-id})
        config (merge (name resource)
                      cli-args)]
    (infof "Connecting to MQTT using %s" config)
    config))

(defn nodeconfig []
  (let [name (keyword (environ/env :name :development))
        resource (edn/read-string (slurp (io/resource "nodeconfig.edn")))
        ; Must be legacy-dns, underscores do not work.
        environ-key-mapping {:legacy-dns :server}
        cli-args (set/rename-keys (select-keys environ/env
                                               (keys environ-key-mapping))
                                  environ-key-mapping)
        config (merge (name resource)
                 cli-args)]
        (infof "Connecting to legacy service using %s" config)
    config))

(defn ddconfig []
  (let [env-name (keyword (environ/env :name :development))
        res (edn/read-string (slurp (io/resource "ddconfig.edn")))
        env-config (env-name res)
        ks (keys env-config)
        config (merge env-config (select-keys environ/env ks))]
    config))

(defn sensorconfig []
  (let [resource (edn/read-string (slurp (io/resource "sensorconfig.edn")))]
    resource))

(defn metrics []
  (let [name (keyword (environ/env :name :development))
        resource (some-> "metrics.edn"
                         io/resource
                         slurp
                         edn/read-string)
        environ-key-mapping {:graphite-service :host}
        cli-args (set/rename-keys (select-keys environ/env
                                               (keys environ-key-mapping))
                                  environ-key-mapping)
        config (merge (name resource)
                      cli-args)]
    (infof "Connecting to Graphite using %s" config)
    config))

(defn rabbitmq []
  (let [name (keyword (environ/env :name :development))
        resource (some-> "rabbitmq.edn"
                         io/resource
                         slurp
                         edn/read-string)
        environ-key-mapping {:rabbitmq-service :host
                             :rabbitmq-user :username
                             :rabbitmq-password :password}
        cli-args (set/rename-keys (select-keys environ/env
                                               (keys environ-key-mapping))
                                  environ-key-mapping)
        config (merge (name resource)
                      cli-args)]
    (infof "Connecting to Rabbitmq using %s" config)
    config))

(defn parse-int
  [int-string]
  (try (Integer/parseInt int-string)
       (catch Throwable t
         (warnf "Cannot parse %s as an integer"
                int-string)
         nil)))

(defn device-service-worker []
  (let [environ-key-mapping {:dsw-consumers :consumer-count
                             :dsw-prefetch :prefetch
                             :dsw-queue-name :queue-name
                             :dsw-exchange :exchange}
                cli-args (set/rename-keys (select-keys environ/env
                                               (keys environ-key-mapping))
                                          environ-key-mapping)
        cli-args (-> cli-args
                     (update :consumer-count parse-int)
                     (update :prefetch parse-int)
                     (->> (remove (fn [[k v]]
                                    (nil? v))))
                     (into {}))
        config (merge {:consumer-count 2
                       :prefetch 2
                       :queue-name "datadealer-device_worker"
                       :exchange "node.events"}
                      cli-args)]
    config))

(defn device-service-control []
  (let [environ-key-mapping {:dsc-exchange :exchange}
                cli-args (set/rename-keys (select-keys environ/env
                                               (keys environ-key-mapping))
                                          environ-key-mapping)
        config (merge {:exchange "node.commands"}
                      cli-args)]
    config))

(defn kafka []
  (let [name (keyword (environ/env :name :development))
        resource (some-> "kafka.edn"
                   io/resource
                   slurp
                   edn/read-string)
        environ-key-mapping {:kafka-service       :host
                             :kafka-port          :port
                             :env-suffix          :suffix
                             :reply-topic         :topic}
        cli-args (set/rename-keys (select-keys environ/env
                                    (keys environ-key-mapping))
                   environ-key-mapping)
        args (->
               cli-args
               (update
                 ;; To handle multiple hosts, take the string
                 ;; and split on `","`.
                 :host #(when %
                          (-> %
                            (.split ",")
                            seq)))
               (remove-empty-from-map))
        config (merge (name resource)
                 args)]

    (infof "Connecting to Kafka using %s" config)
    config))

(defn kafka2 []
  (let [name (keyword (environ/env :name :development))
        resource (some-> "kafka2.edn"
                         io/resource
                         slurp
                         edn/read-string)
        environ-key-mapping {:kafka-service :host
                             :kafka-port    :port
                             :env-suffix    :suffix
                             :gps-topic     :gps-topic
                             :alarm-topic   :alarm-topic}
        cli-args (set/rename-keys (select-keys environ/env
                                               (keys environ-key-mapping))
                                  environ-key-mapping)
        args (->
               cli-args
               (update
                 ;; To handle multiple hosts, take the string
                 ;; and split on `","`.
                 :host #(when %
                          (-> %
                              (.split ",")
                              seq)))
               (remove-empty-from-map))
        config (merge (name resource)
                      args)]

    (infof "Connecting to Kafka using %s" config)
    config))

(defn kafkacape []
  (let [name (keyword (environ/env :name :development))
        resource (some-> "kafkacape.edn"
                   io/resource
                   slurp
                   edn/read-string)
        environ-key-mapping {:kafka-service :host
                             :kafka-group   :group
                             :kafka-va-group :va-group
                             :kafka-port    :port
                             :env-suffix    :suffix
                             :device-topic-event  :device-topic-event
                             :device-topic-config :device-topic-config
                             :device-topic-out    :device-topic-out
                             :schedule-topic      :schedule-topic
                             :trigger-topic       :trigger-topic
                             :trigger-lux-topic   :trigger-lux-topic
                             :presence-topic      :presence-topic
                             :driver-level-topic  :driver-level-topic
                             :ambient-light-topic :ambient-light-topic
                             :reply-topic   :topic}
        cli-args (set/rename-keys (select-keys environ/env
                                    (keys environ-key-mapping))
                   environ-key-mapping)
        args (->
               cli-args
               (update
                 ;; To handle multiple hosts, take the string
                 ;; and split on `","`.
                 :host #(when %
                          (-> %
                            (.split ",")
                            seq)))
               (remove-empty-from-map))
        config (merge (name resource)
                 args)]

    (infof "Connecting to Kafka using %s" config)
    config))

(defn zookeepercape []
  (let [name (keyword (environ/env :name :development))
        resource (some-> "zookeepercape.edn"
                   io/resource
                   slurp
                   edn/read-string)
        environ-key-mapping {:zookeeper-service :host
                             :zookeeper-port    :port
                             :env-suffix        :suffix
                             :request-topic     :topic}
        cli-args (set/rename-keys (select-keys environ/env
                                    (keys environ-key-mapping))
                   environ-key-mapping)
        args (->
               cli-args
               (update
                 ;; To handle multiple hosts, take the string
                 ;; and split on `","`.
                 :host #(when %
                          (-> %
                            (.split ",")
                            seq)))
               (remove-empty-from-map))
        config (merge (name resource)
                 args)]

    (infof "Connecting to Zookeeper using %s" config)
    config))

(defn zookeeper []
  (let [name (keyword (environ/env :name :development))
        resource (some-> "zookeeper.edn"
                   io/resource
                   slurp
                   edn/read-string)
        environ-key-mapping {:zookeeper-service :host
                             :zookeeper-port    :port
                             :env-suffix        :suffix
                             :consumer-group    :group
                             :request-topic     :topic}
        cli-args (set/rename-keys (select-keys environ/env
                                    (keys environ-key-mapping))
                   environ-key-mapping)
        args (->
               cli-args
               (update
                 ;; To handle multiple hosts, take the string
                 ;; and split on `","`.
                 :host #(when %
                          (-> %
                            (.split ",")
                            seq)))
               (remove-empty-from-map))
        config (merge (name resource)
                 args)]

    (infof "Connecting to Zookeeper using %s" config)
    config))

(defn calculate-token
  [config]
  (let [kvpairs (into (sorted-map) (dissoc (keywordize-keys config) :configid :name :nodes :model))
        token (format "%x" (digest/crc32 (json/write-str kvpairs)))]
    {:token token :kvpairs kvpairs}))
