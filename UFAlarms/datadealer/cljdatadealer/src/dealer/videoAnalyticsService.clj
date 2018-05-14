(ns ^{:doc "videoAnalytics Service."}
  dealer.videoAnalyticsService
  (:gen-class)
  (:require [clojure.tools.logging :refer :all]
            [clojure.tools.nrepl.server :as nrepl]
            [com.stuartsierra.component :as component]
            [dealer
             [metrics :as metrics]
             [videoAnalyticsWorker :as worker]]
            [kafka.consumer :as consumer]
            [kafka.producer :as producer]
            [neowrap.neowrapper :as neo4j]
            [environ.core :as environ]
            [utils 
             [config :as config]
             [cassandra_intf :as cass]]))

(def config
  (delay
    (let [config {:metrics (config/metrics)
                  :kafka (config/kafkacape)}]
      (spy :info config))))

(def system
  (-> (component/system-map
        :metrics-registry (metrics/new-metrics-registry "va")
        :jmx-reporter (metrics/new-jmx-reporter)
        :graphite-reporter (metrics/new-graphite-reporter (:metrics @config))
        :jvm-instrumenter (metrics/new-jvm-instrumenter)
        :kafka-connector (consumer/new-kafka-consumer (:kafka @config))
        :kafka-producer (producer/new-kafka-producer (:kafka @config))
        :neo4j (neo4j/new-neo4j)
        :cassandra (cass/new-cassandra)
        :video-analytics-worker (worker/new-videoAnalyticsWorker))
    (component/system-using {:graphite-reporter [:metrics-registry]
                             :jmx-reporter [:metrics-registry]
                             :jvm-instrumenter [:metrics-registry]
                             :cassandra [:metrics-registry]
                             :neo4j [:metrics-registry]
                             :video-analytics-worker [:kafka-connector
                                                      :neo4j
                                                      :cassandra]})
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
    (infof "Running video Analytics Service version: %s"
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

  (nrepl/start-server :port 7890)
  (info "Starting system.")
  (system-start)
  (doto "System fully started."
    info
    println))
