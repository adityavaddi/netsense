(ns ^{:doc "Acl Service."}
  dealer.aclservice
  (:gen-class)
  (:require [clojure.tools.logging :refer :all]
            [clojure.tools.nrepl.server :as nrepl]
            [com.stuartsierra.component :as component]
            [kafka
             [consumer :as consumer]
             [producer :as producer]]
            [dealer
             [metrics :as metrics]
             [aclworker :as aclworker]]
            [neowrap.neowrapper :as neo4j]
            [environ.core :as environ]
            [utils [config :as config]]))

(def config
  (delay
    (let [config {:metrics (config/metrics)
                  :kafka (config/kafka)
                  :zookeeper (config/zookeeper)}]
      (spy :info config))))

(def system
  (-> (component/system-map
        :metrics-registry (metrics/new-metrics-registry "acl")
        :jmx-reporter (metrics/new-jmx-reporter)
        :graphite-reporter (metrics/new-graphite-reporter (:metrics @config))
        :jvm-instrumenter (metrics/new-jvm-instrumenter)
        :neo4j (neo4j/new-neo4j)
        ;:producer (producer/new-kafka-producer (:kafka @config))
        ;:consumer (consumer/new-kafka-consumer (:kafka @config))
        :acl-service (aclworker/new-aclworker @config))
    (component/system-using {:graphite-reporter [:metrics-registry]
                             :jmx-reporter [:metrics-registry]
                             :jvm-instrumenter [:metrics-registry]
                             :neo4j [:metrics-registry]
                             :acl-service [:metrics-registry
                                                     ]})
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
    (infof "Running Acl Service version: %s"
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

  (nrepl/start-server :port 7889)
  (info "Starting system.")
  (system-start)
  (doto "System fully started."
    info
    println))
