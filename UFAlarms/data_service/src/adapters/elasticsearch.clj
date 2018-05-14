(ns adapters.elasticsearch
  (:require [adapters.core]
            [clj-http.client :as client]
            [clojure.data.json :as json]
            [clojurewerkz.elastisch.native :as es]
            [clojurewerkz.elastisch.native.document :as esd]))

(defn not-nil [value]
  (assert (instance? Object value)) value)

(defn option-to-string [value]
  (.toString ^Object (not-nil value)))

(defn get-cluster-name [hostname]
  (let [url (format "http://%s:9200/_nodes/cluster" hostname)]
    (option-to-string (get (json/read-str (not-nil (get (client/get url) :body))) "cluster_name"))))

(defn elasticsearch-event-consumer-init [ip]
  [[] (es/connect  [[ip 9300]] {"cluster.name" (get-cluster-name ip)})])

(defn flatten-one [col] (vec (mapcat #(if (sequential? %) % [%]) col)))
(defn flatten-two [col] (-> col flatten-one flatten-one))

(defn flatten-json [json-map]
  (let [remove-empty-prefix (fn [vct] (if (= "" (first vct)) (vec (rest vct)) vct))
        join-prefix (fn [prefix k] (clojure.string/join "." (remove-empty-prefix [prefix k])))
        wrap-vector #(if (sequential? %) [%] %)
        inner (fn flatten-recur [mp prefix]
                (let [m (apply hash-map (flatten-two (for [[k v] mp]
                  (if (instance? clojure.lang.IPersistentMap v)
                    (vec (seq (flatten-recur v (join-prefix prefix (name (option-to-string k))))))
                    [(join-prefix prefix (name (option-to-string k))) (wrap-vector v)]))))]
                  m))]
    (inner json-map "")))

(defn ElasticsearchEventConsumer [ip]
  (let [conn (es/connect  [[ip 9300]] {"cluster.name" (get-cluster-name ip)})]
    (reify adapters.core/EventConsumer
      (consume [_ event]
        (let [logevent-map (adapters.core/flatten-uniqueLogEvent event)
              flattened-msg (flatten-json (json/read-str (option-to-string (get logevent-map :message))))
              merged (merge {} flattened-msg (assoc-in (dissoc logevent-map :message) [:key]
               (option-to-string (:key logevent-map))))
              print-error (fn [excp] (println (format "Error payload: %s, Merged: %s, Exception: %s"
                (:message (:log-event event)) merged excp)) excp)
              key-str (option-to-string (get merged :key))]
            (try (esd/put conn
                "farallones_logevents" "logevent" key-str (json/read-str (json/write-str merged)))
            (catch Exception e (print-error (option-to-string e)))))))))

