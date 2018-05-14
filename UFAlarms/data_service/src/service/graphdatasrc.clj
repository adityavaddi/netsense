(ns service.graphdatasrc
  (:require [clojure.core.match :refer [match]]
            [common.core]
            [clojurewerkz.neocons.rest.relationships :as nrl]
            [clojurewerkz.neocons.rest.nodes :as nn]
            [clojurewerkz.neocons.rest.index :as idx]
            [clojurewerkz.neocons.rest.labels :as lbl]
            [clojurewerkz.neocons.rest :as nr]
            [clojurewerkz.neocons.rest.helpers :as hlp]
            [clojure.data.json :as json]
            [service.datasrc])
  (:import (clojure.lang PersistentVector ExceptionInfo)))

(defn- get-relationships [lbl ref-fields instance]
  (let [res (for [field ref-fields]
              (let [field-name (get field :name)
                    value (get instance (keyword field-name))]
                [lbl (get instance :id) (common.core/singularize field-name)
                 [(common.core/lowercase-first (get field :type))
                  (if (get field :list?) value [value])]]))] res))

(defn- create-node-with-data [conn label data]
  (let [node (nn/create conn data)]
    (lbl/add conn node label) node))

(defn- get-reference-fields [fields]
  (let [primitives #{"Boolean" "String" "Float" "Int"}
        fields-grouped (group-by #(contains? primitives (get % :type)) fields)
        [_ reference-fields] [(get fields-grouped true) (get fields-grouped false)]] reference-fields))

(defn- entity->label [entity-name] (keyword (common.core/lowercase-first entity-name)))

(defn- create-node
  ([conn label reference-fields data]
   (let [primitive-values (apply (partial dissoc data) (map (comp keyword :name) reference-fields))
         node (create-node-with-data conn label primitive-values)] node))
  ([conn entity data]
   (let [label (entity->label entity)]
     (create-node conn label (get-reference-fields (first (service.datasrc/get-descriptors entity nil))) data))))

(defn- load-instance [conn label reference-fields instance]
  (let [node (create-node conn label reference-fields instance)]
    [{(get instance :id) node} (get-relationships label (remove #(= "ID" (get % :type)) reference-fields) instance)]))

(defn- load-entity [conn [fields _ _ required entity _] instances]
  (let [reference-fields (get-reference-fields fields)]
    (let [label (common.core/singularize entity)
          loaded (map (partial load-instance conn label reference-fields) instances)
          [maps refs] [(map first loaded) (map second loaded)]]
      [(apply merge {} maps) refs])))

(defn- reset-graph [conn]
  (let [labels (lbl/get-all-labels conn)]
    (doseq [label labels] (nn/destroy-many conn (lbl/get-all-nodes conn label)))))

(defn- load-seed-data [conn data]
  (println "Resetting database and loading seed data...")
  (reset-graph conn)
  (let [to-schema (fn [e] (common.core/capitalize-first (common.core/drop-last-char (name e))))
        nodes (atom {})
        relations (atom [])]
    (doseq [[entity instances] data]
      (let [[node-map refs] (load-entity conn (service.datasrc/get-descriptors (to-schema entity) nil) instances)]
        (swap! nodes assoc entity node-map)
        (swap! relations conj refs)))
    (doseq [[from from-id rel-name [to to-ids]] (apply concat (apply concat @relations))
            :let [get-node (fn [entity id] (get (get @nodes (common.core/pluralize entity)) id))]
            to-id to-ids]
      (let [from-node (get-node from from-id)
            [to to-id] (cond
                         (= PersistentVector (type to-id)) to-id
                         (.contains to-id "$") (clojure.string/split to-id #"\$")
                         :else [to to-id])
            to-node (get-node (entity->label (name to)) to-id)]
        (if (and from-node to-node)
          (nrl/create conn from-node to-node rel-name)
          (println (format "Skipping spurious relationship: %s:%s->%s:%s" (name from) from-id to to-id)))))))

(defn- extract-relationships [conn node]
  (let [grouped (group-by first (map (fn [n] [(:type n) (:end n)]) (nrl/outgoing-for conn node)))
        consolidated (for [[k v] grouped] [k  (map hlp/extract-id (map second v))])
        label (first (lbl/get-all-labels conn node))
        [fields _ _ _ _ _] (service.datasrc/get-descriptors (common.core/capitalize-first (name label)) nil)
        list-fields (set (map :name (filter #(get % :list?) fields)))]
    (into {} (for [[k v] consolidated]
      (if (contains? list-fields (common.core/pluralize k)) [(common.core/pluralize k) (vec v)] [k (first v)])))))
(defn- extract-from-node [conn node] (merge {:id (str (:id node))} (:data node) (extract-relationships conn node)))

(defn- try-expecting [quoted-block expected msg]
  (try (eval quoted-block)
  (catch ExceptionInfo info
    (let [excp (get (json/read-str (:body (.data info))) "exception")]
      (if (= expected excp)
        (assert false msg)
        (throw info))))))

(defn- get-node-by-id
  ([conn id-str] (get-node-by-id conn id-str "Cannot retrieve non-existent node."))
  ([conn id-str msg]
   (try-expecting `(nn/get ~conn ~(Integer/parseInt id-str)) "NodeNotFoundException"
                  (format "%s ID: %s" msg id-str))))

(defn- assert-is-of-type [conn node type msg]
  (let [lbl (first (lbl/get-all-labels conn node))]
    (assert (= type lbl) (format "%s Expected: %s, Actual: %s" msg type lbl))))

(defn- create-relationship [conn from-node to-node-id label to-type]
  (let [to-node (get-node-by-id conn to-node-id "Cannot create relationship to non-existent node.")]
    (assert-is-of-type conn to-node to-type
      (format "Cannot create relationship for field: %s to wrong type." (name label)))
    (nrl/create conn from-node to-node label)))

(def GraphDataSource
  (let [seed-data (common.core/get-config "stubdata.edn")
        config (get-in (common.core/get-config) [:databases :neo4j])
        [username password ip] (map #(get config %) [:username :password :ip])
        conn (nr/connect (format "http://%s:%s@%s:7474/db/data" username password ip))
        expected-counts (for [[k v] seed-data] [(common.core/singularize (name k)) (count v)])
        actual-at-least-expected? (fn [[lbl cnt]] (>= (count (lbl/get-all-nodes conn lbl)) cnt))]
    (reify service.datasrc/DataSource
      (service.datasrc/schema [_ enums objs unions]
        (if (not (every? actual-at-least-expected? expected-counts)) (load-seed-data conn seed-data)))
      (service.datasrc/select [_ entity fltr]
        (let [label (keyword (common.core/lowercase-first entity))
              [field value] (clojure.string/split fltr #"=")]
          (if (= field "id")
            (extract-from-node conn (get-node-by-id conn value))
            (map (partial extract-from-node conn)
                 (apply (partial lbl/get-all-nodes conn label) (if (not= fltr "all()") [field value]))))))
      (service.datasrc/insert [_ entity params]
        (let [[fields _ _ required _ _] (service.datasrc/get-descriptors entity params)
              required (map (comp keyword :name) required)]
          (assert (every? #(contains? (set (keys params)) %) required)
            (format "Missing required param. Required: %s, Provided: %s" (apply str required) params))
          (let [node (create-node conn entity params)
                properties (set (keys (:data node)))
                relationships (remove #(contains? properties (first %)) params)]
            (doseq [[relation id] relationships]
              (create-relationship conn node id relation
                (entity->label (:type (common.core/single-or-nil (filter #(= relation (keyword (:name %))) fields))))))
            (merge {:id (str (:id node))} (:data node) (into {} relationships)))))
      (service.datasrc/modify [_ entity params] nil)
      (service.datasrc/delete [_ entity id]
        (let [node (get-node-by-id conn id)
              extracted (extract-from-node conn node)]
          (assert-is-of-type conn node (entity->label entity) "Cannot destroy node of incorrect type.")
          (nn/destroy conn node) extracted)))))

