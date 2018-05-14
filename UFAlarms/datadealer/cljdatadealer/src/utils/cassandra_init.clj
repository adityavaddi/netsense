(ns ^{:doc "The Cassandra Preparator."
      :author "Chiradip Mandal"}
  utils.cassandra_init (:gen-class)
  (:require
            [com.stuartsierra.component :as component]
            [dealer.metrics :as metrics]
            [utils.cassandra-cql :as cql]
            [utils.cassandra_intf :as cass
             :refer [conn]]
            [clojure.java.shell :as shell])
  (:use clojure.tools.logging)
  (:require [utils.config :as conf]))

(defn load_tables [key_space]
  (try
    (let [ res_ate (shell/sh "cqlsh" (str "-e COPY " key_space ".aggregation_traffic_events (siteid,nodeid,aggregation_type,starttime,type,eventid,objectclass,avgdwell,avgvelocity,enddt,endtime,eventcnt,medianvelocity,p85velocity,startday,startdt,starthr,ts) FROM 'aggregation_traffic_events.csv';") :dir "../../api_tests/data/cassandra/")
           res_ates (shell/sh "cqlsh" (str "-e COPY " key_space ".aggregation_traffic_events_site (siteid,aggregation_type,starttime,type,objectclass,avgdwell,avgvelocity,enddt,endtime,eventcnt,medianvelocity,p85velocity,startday,startdt,starthr,ts) FROM 'aggregation_traffic_events_site.csv';") :dir "../../api_tests/data/cassandra/")
           res_ts (shell/sh "cqlsh" (str "-e COPY " key_space ".traffic_status  (date, nodeid, time, active, channel, count, data, type, orgid, siteid, tags, trafficdetectioneventid,  updated) FROM 'traffic_status.csv';") :dir "../../api_tests/data/cassandra/")
           res_tcs (shell/sh "cqlsh" (str "-e COPY " key_space ".traffic_current_status (nodeid, time, active, channel, count, data, type, orgid, siteid, tags, trafficdetectioneventid, updated, aggregated_count) FROM 'traffic_current_status.csv';") :dir "../../api_tests/data/cassandra/")
           res_tc (shell/sh "cqlsh" (str "-e COPY " key_space ".traffic_config (eventid, active, channel, data, type, nodeid, orgid, siteid, tags, time, updated) FROM 'traffic_config.csv';") :dir "../../api_tests/data/cassandra/")
           res_dss (shell/sh "cqlsh" (str "-e COPY " key_space ".device_sensor_samples FROM 'device_sensor_samples.csv';") :dir "../../api_tests/data/cassandra/")
           res_pzs (shell/sh "cqlsh" (str "-e COPY " key_space ".parking_zone_status (siteid,parkingzoneid,config,orgid,parkinggroupid,state,type) FROM 'parking_zone_status.csv';") :dir "../../api_tests/data/cassandra/")
           ]
      ;(println (if (> (:exit res_ate) 0)  (str "Error:" (:err res_ate)) (str "Done: " (:out res_ate))))
      (if (> (:exit res_ate) 0) (throw (ex-info
                                         (str "Could not load table aggregation_traffic_events: " (:err res_ate))
                                         {:message (:err res_ate) :cause "Error load table aggregation_traffic_events"})) (spy :debug (:out res_ate)))
      ;(println (if (> (:exit res_ate) 0)  (str "Error:" (:err res_ates)) (str "Done: " (:out res_ates))))
      (if (> (:exit res_ates) 0) (throw (ex-info
                                         (str "Could not load table aggregation_traffic_events_site: " (:err res_ates))
                                         {:message (:err res_ates) :cause "Error load table aggregation_traffic_events_site"})) (spy :debug (:out res_ates)))
      ;(println (if (> (:exit res_ts) 0)  (str "Error:" (:err res_ts)) (str "Done: " (:out res_ts))))
      (if (> (:exit res_ts) 0) (throw (ex-info
                                         (str "Could not load table traffic_status: " (:err res_ts))
                                         {:message (:err res_ts) :cause "Error load table traffic_status"})) (spy :debug (:out res_ts)))
      ;(println (if (> (:exit res_tcs) 0)  (str "Error:" (:err res_tcs)) (str "Done: " (:out res_tcs))))
      (if (> (:exit res_tcs) 0) (throw (ex-info
                                         (str "Could not load table traffic_current_status: " (:err res_tcs))
                                         {:message (:err res_tcs) :cause "Error load table traffic_current_status"})) (spy :debug (:out res_tcs)))
      ;(println (if (> (:exit res_dss) 0)  (str "Error:" (:err res_dss)) (str "Done: " (:out res_dss))))
      (if (> (:exit res_tc) 0) (throw (ex-info
                                         (str "Could not load table traffic_config: " (:err res_tc))
                                         {:message (:err res_tc) :cause "Error load table traffic_config"})) (spy :debug (:out res_tc)))
      (if (> (:exit res_dss) 0) (throw (ex-info
                                        (str "Could not load table device_sensor_samples: " (:err res_dss))
                                        {:message (:err res_dss) :cause "Error load table device_sensor_samples"})) (spy :debug (:out res_dss)))
      (if (> (:exit res_pzs) 0) (throw (ex-info
                                        (str "Could not load table parking_zone_status: " (:err res_pzs))
                                        {:message (:err res_pzs) :cause "Error load table parking_zone_status"})) (spy :debug (:out res_pzs)))
      )

;    (println (str "Importing data into " key_space))
;    ;(println (shell/sh "cqlsh" "-e DESCRIBE farallones_test.aggregation_traffic_events;" :dir "/home/dejan/Documents/"))
;    (println (shell/sh "cqlsh" (str "-e COPY " key_space ".aggregation_traffic_events FROM 'aggregation_traffic_events.csv';") :dir "../../api_tests/data/cassandra/"))
;    (println (shell/sh "cqlsh" (str "-e SELECT * FROM " key_space ".aggregation_traffic_events LIMIT 10;" )))
;    (println (shell/sh "cqlsh" (str "-e SELECT * FROM " key_space ".device_sensor_samples LIMIT 10;" )))
;    ;(cql/use-keyspace @conn key_space)
;    ;(cc/execute @conn "SELECT * FROM farallones_test.aggregation_traffic_events; ")
;    (println "Done Importing data")

    (catch Exception e
      (error "Exception loading data")
      (doto e
        error
        throw))))

(defn initialize_tables []
  (let [config (conf/cassservice)
        keyspace (:keyspace config)]
    (try
      (let [result (shell/sh  "cqlsh" "-k" keyspace "-f" "create.cql" :dir "../../dbinit/cql") ]
        (if (> (:exit result) 0)
          (throw (ex-info
                  (format "Could not load tables from create.cql %s" (:err result))
                  {:message (:err result) :cause "Error creating Cassandra tables"})) (:out result)))
      (catch Exception e
        (error "Exception creating Cassandra tables")
        (doto e
              error
              throw)))))

(defn load_data []
  (let [config (conf/cassservice)
        keyspace (:keyspace config)]
    (load_tables keyspace)))

(def system
  (-> (component/system-map
       :cassandra (cass/new-cassandra)
       :metrics-registry (metrics/new-metrics-registry "cass")
       :jmx-reporter (metrics/new-jmx-reporter)
       :jvm-instrumenter (metrics/new-jvm-instrumenter))
      (component/system-using {
                               :cassandra [:metrics-registry]
                               :jmx-reporter [:metrics-registry]
                               :jvm-instrumenter [:metrics-registry]})
      delay))

(defn system-start []
  (alter-var-root #'system
                  (fn [system]
                    (component/start
                     (if (delay? system)
                       @system
                       system)))))