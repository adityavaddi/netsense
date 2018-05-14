(ns utils.nodeconfig (:gen-class)
    (:use clojure.tools.logging)
    (:require [neowrap.neowrapper :as neo4j])
    (:require [utils.config :as config]
              [clojure.set :refer :all]
              [clojure.walk :refer :all]
              [clojure.string :as str]))

(def sensors (config/sensorconfig))

(defn default-sensor-config-by-model [model]
  (get {"unode-v2" (:sensor-config-defaults-v2 sensors)
        "unode-v3" (:sensor-config-defaults-v3 sensors)
        "unode-v4" (:sensor-config-defaults-v4 sensors)
        "unode-v5" (:sensor-config-defaults-v5 sensors)
        "unode-v6" (:sensor-config-defaults-v6 sensors)} model))

(defn default-sensor-config [model]
  (merge (:sensor-config-defaults sensors) (default-sensor-config-by-model model)))

(defn default-node-config []
  (config/nodeconfig))

(defn translate-config [conf]
  (->> (for [[k v] conf]
         [(get (:sensor-name-translator sensors) (keyword k)
               (-> k
                   name
                   (.replace \_ \.)
                   keyword))
          v])
       (into {})))

(defn translate-sensor-id [sensorid]
  (let [sensormap {:RF :rf
                   :sn :rf
                   :bRA :bR
                   :bRL :bR
                   :jty :jt
                   :jtx :jt
                   :jtz :jt
                   :jtm :jt}]
    (name ((keyword sensorid) sensormap sensorid))))

(defn encode-config [conf]
  (->> (for [[k v] conf]
         [(get (map-invert (:sensor-name-translator sensors)) (keyword k)
               (-> k
                   name
                   (.replace \. \_)
                   keyword))
          v])
       (into {})))

(defn get-full-conf-by-model [model]
  (let [sensor-conf (default-sensor-config model)
        node-conf (get {"unode-v2" (default-node-config)
                        "unode-v3" (default-node-config)
                        "unode-v4" (default-node-config)
                        "unode-v5" (dissoc (default-node-config)
                                           :networkXSSID
                                           :networkXSecurity
                                           :networkXPasskey
                                           :networkYSSID
                                           :networkYSecurity
                                           :networkYPasskey)
                        "unode-v6" (dissoc (default-node-config)
                                           :networkXSSID
                                           :networkXSecurity
                                           :networkXPasskey
                                           :networkYSSID
                                           :networkYSecurity
                                           :networkYPasskey)} model)
        full-conf (merge sensor-conf node-conf)]
    (encode-config full-conf)))

