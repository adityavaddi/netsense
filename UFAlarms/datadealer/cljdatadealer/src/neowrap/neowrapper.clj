(ns ^{:doc "The Neo4J Clojure Wrapper."}
    neowrap.neowrapper
  (:gen-class)
  (:require [clojure.tools.logging :refer :all]
            [com.stuartsierra.component :as component]
            [environ.core :as environ]
            [metrics.timers
             :as timers
             :refer [start stop]
             :rename {start start-timer
                      stop stop-timer}]
            [utils.config :as config]
            [me.raynes.fs :as fs])
  (:import [com.sensity.netsense.neo CypherEmbeddedImpl CypherImpl]
           [java.util HashMap Map]))

(def neo-timer-name "neo4j-timer")
(defonce neo-timer (timers/timer neo-timer-name))

(def hosts (:hosts (config/neo4jservice)))
(def port (:port (config/neo4jservice)))
(def db-path (:dbpath (config/neo4jservice)))

(def conn
  (delay
    (infof "Trying to connect to Neo4j %s" hosts)
    (if-let [connected (try
                         (CypherImpl. hosts port)
                         (catch Exception e
                           (spyf :error "Neo4j connection failed: %s" e)
                           false))]
      connected
      (do
        (errorf "Could not connect to Neo4j. Sleeping %d, then retrying." 250)
        (Thread/sleep 250)
        (recur)))))

(def cypherExec
  (let [env-name (keyword (environ/env :name :development))]
    (case env-name
      :production conn
      :docker conn
      :development (delay (CypherEmbeddedImpl. db-path))
      (atom nil))))

(defn start
  ([]
   (info "Starting Neo4j.")
   (when (instance? CypherImpl @cypherExec)
     (let [ret (.getDbPath @cypherExec)]
          ret
          (info "Neo4j Started."))))
  ([baseline path]
   (do 
     (fs/delete-dir path)
     (fs/copy-dir baseline path)
     (when (instance? clojure.lang.Atom cypherExec)
       (reset! cypherExec (CypherEmbeddedImpl. path)))
     (.getDbPath @cypherExec))))

(defn stop []
  (when (not (nil? @cypherExec))
      (.stop @cypherExec)))


(defn executeQuery
  ([cypher]
   (executeQuery cypher nil cypherExec))
  ([cypher ^Map opts]
   (executeQuery cypher opts cypherExec))
  ([cypher ^Map opts & [cypher-instance]]
   (let [timer (start-timer neo-timer)]
     (try
       (if (nil? opts) 
          (.executeCypher @cypher-instance cypher)
          (let [hashmap (clojure.walk/postwalk (fn [x]
                                                 (if-not (map? x)
                                                   x
                                                   (HashMap. x)))
                                               opts)]
            (.executeCypher @cypher-instance cypher hashmap)))
       (finally (stop-timer timer))))))


(defrecord Neo4j [metrics-registry]
  component/Lifecycle
  (start [component]
    (let [{:keys [registry
                  prefix]} metrics-registry]
      (alter-var-root #'neo-timer
                      (fn [& _]
                        (timers/timer registry
                                      (conj prefix neo-timer-name)))))
    (start)
    component)
  (stop [component]
    (info "Stopping Neo4j.")))

(defn new-neo4j
  []
  (map->Neo4j {}))
