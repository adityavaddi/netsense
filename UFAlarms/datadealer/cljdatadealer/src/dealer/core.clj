(ns ^{:doc "Data Dealer Core"}
    dealer.core
  (:gen-class)
  (:require [amqp.core :as amqp]
            [appl
             [et-daylight-harvesting :as etdh]
             [daylight_harvest :as dh]
             [proximity-dimming :as pd]]
            [clojure.tools.logging :refer :all]
            [clojure.tools.nrepl.server :as nrepl]
            [com.stuartsierra.component :as component]
            [kafka.consumer :as consumer]
            [kafka.producer :as producer]
            [dealer
             [actlogworker :as alw]
             [dealer :as dealer]
             [devsvcctrl :as dsc]
             [devsvcworker :as dsw]
             [devsvcworker-kafka :as dswk]
             [metrics :as metrics]
             [mqttlistener :as listener]
             [worker :as worker]
             [capeworker :as capeworker]]
            [environ.core :as environ]
            [mqtt.core :as mqtt]
            [neowrap.neowrapper :as neo4j]
            [kafka.producer :as producer]
            [utils
             [cape :as cape]
             [cassandra_intf :as cass]
             [config :as config]
             [schedules :as schedules]]))

(def config
  (delay
   (let [config {:metrics (config/metrics)
                 :rabbitmq (config/rabbitmq)
                 :device-service-worker (config/device-service-worker)
                 :device-service-control (config/device-service-control)
                 :kafka (config/kafkacape)
                 :kafka2 (config/kafka2)
                 :zookeeper (config/zookeepercape)}]
     (spy :info config))))

(def system
  (-> (component/system-map
       :metrics-registry (metrics/new-metrics-registry "dd")
       :jmx-reporter (metrics/new-jmx-reporter)
       :graphite-reporter (metrics/new-graphite-reporter (:metrics @config))
       :jvm-instrumenter (metrics/new-jvm-instrumenter)
       :cassandra (cass/new-cassandra)
       :neo4j (neo4j/new-neo4j)
       :mqtt (mqtt/new-mqtt)
       :amqp-connection (amqp/new-amqp-connection (:rabbitmq @config))
       :activity-log-worker (alw/new-activity-log-worker)
       :kafka2-producer (producer/new-kafka-producer (:kafka2 @config))
       :device-service-worker (dsw/new-device-service-worker (:device-service-worker @config))
       :device-service-control (dsc/new-device-service-control (:device-service-control @config))
       :kafka-connector (consumer/new-kafka-consumer (:kafka @config))
       :kafka-sensor-connector (consumer/new-kafka-sensor-consumer (:kafka @config))
       :device-service-worker-kafka (dswk/new-device-service-worker-kafka (:kafka @config))
       :dealer (dealer/new-dealer)
       :worker (worker/new-worker)
       :mqtt-listener (listener/new-mqtt-listener)
       :cape (cape/new-cape)
       :capedevice-worker (capeworker/new-capeworker @config)
       :schedule-updater (schedules/new-schedule-updater)
       :kafka-producer (producer/new-kafka-producer (:kafka @config))
       ;:etdh-scheduler (etdh/new-etdh-scheduler)
       ;:dh-scheduler (dh/new-dh-scheduler)
       :pd-scheduler (pd/new-pd-scheduler (:pd-interval environ/env)))
      (component/system-using {:graphite-reporter [:metrics-registry]
                               :jmx-reporter [:metrics-registry]
                               :jvm-instrumenter [:metrics-registry]
                               :cassandra [:metrics-registry]
                               :neo4j [:metrics-registry]
                               :device-service-worker [:metrics-registry
                                                       :amqp-connection]
                               :device-service-control [:metrics-registry
                                                        :amqp-connection]
                               :device-service-worker-kafka [:kafka-sensor-connector
                                                             :neo4j
                                                             :cassandra]
                               :dealer [:metrics-registry]
                               :worker [:metrics-registry]
                               :cape [:amqp-connection
                                      :metrics-registry]
                               ;:etdh-scheduler [:metrics-registry]
                               :amqp-connection [:neo4j
                                                 :cassandra]
                               :pd-scheduler [:metrics-registry]})
      delay))


(defn system-start []
  (alter-var-root #'system
                  (fn [system]
                    (component/start
                     (if (delay? system)
                       @system
                       system)))))

(defn system-stop []
  (alter-var-root #'system
                  (fn [s]
                    (when s
                      (component/stop s)))))

(defn -main [& _]

  (when-let [git-describe (clojure.java.io/resource "git-describe")]
    (infof "Running Datadealer version: %s"
            (slurp git-describe)))

  (Thread/setDefaultUncaughtExceptionHandler
   (reify Thread$UncaughtExceptionHandler
     (uncaughtException [_ thread ex]
       (error ex "Uncaught exception on" (.getName thread))
       (error ex))))

  (.addShutdownHook
   (Runtime/getRuntime)
   (Thread. (fn []
              (info "Shutting down thread pool for Clojure agents and futures.")
              (shutdown-agents)
              (info "Shutting down system...")
              (system-stop)
              (info "System fully stopped."))))

  @cass/conn
  (nrepl/start-server :port 7888)
  (info "Starting system.")
  (system-start)
  (doto "System fully started."
    info
    println))
