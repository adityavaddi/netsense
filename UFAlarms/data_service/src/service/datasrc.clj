(ns service.datasrc
  (:require [clojure.core.match :refer [match]]
            [clojure.data.json :as json]
            [common.core]
            [clojure.walk :as walk]))

(defprotocol DataSource
  (schema [this enums objs unions])
  (select [this entity fltr])
  (insert [this entity params])
  (modify [this entity params])
  (delete [this entity id]))

(def ^:private enums (atom {}))
(def ^:private objs (atom {}))
(def ^:private unions (atom {}))

(defn get-key [entity]
  (keyword (common.core/pluralize (common.core/transform-first-char entity common.core/to-lower-case))))

(defn get-descriptors [entity params]
  (let [fields (get @objs entity)
       sans-id (remove #(= "id" (get % :name)) fields)
       fieldnames (set (map (comp keyword :name) fields))
       required (filter #(get % :not-null?) sans-id)
       obj-key (get-key entity)
       trimmed (into {} (filter #(contains? fieldnames (first %)) params))]
   [fields sans-id fieldnames required obj-key trimmed]))

(defn- extract-fields [params]
  (map #(apply assoc {} (interleave [:name :type :list? :not-null?] %)) params))

(defn- handle-schema-rec [uid params]
  (let [[obj-type typename params-str] (clojure.string/split params #"\|")
        params (read-string params-str)]
    (println (format "Received schema: uid: %s, type: %s, typename: %s, params: %s" uid obj-type typename params))
    (match [obj-type]
      ["obj"] (swap! objs assoc typename (extract-fields params))
      ["union"] (swap! unions assoc typename (vec params))
      ["enum"] (swap! enums assoc typename (apply hash-map (flatten params))))))

(def ^:private data-source (atom nil))
(defn set-data-source [datasrc] (swap! data-source (fn [_] datasrc)))

(defn handle-schema-batch [uid params]
  (let [rows (clojure.string/split params #"\!")]
    (doall (map (partial handle-schema-rec uid) rows))
    (schema @data-source enums objs unions)
    (json/write-str {:result :success})))

(defn handle-insert [uid entity params]
  (println (format "Received insert request: uid: %s, entity: %s, params: %s" uid entity params))
  (assert (contains? @objs entity) (format "Cannot insert unknown entity: %s, objs: %s" entity @objs))
  (insert @data-source entity (walk/keywordize-keys (read-string params))))

(defn handle-select [uid entity fltr]
  (println (format "Received select request: uid: %s, entity: %s, filter: %s" uid entity fltr))
  (let [[entity fltr] (if (contains? @unions entity)
                        (let [[attr value] (clojure.string/split fltr #"=")]
                          (assert (= attr "id") (format "Can only select union references by id. Attempted by: %s" attr))
                          (let [union-desc (walk/keywordize-keys (read-string value))]
                            [(get union-desc :type) (format "id=%s" (get union-desc :id))])) [entity fltr])]
    (select @data-source entity fltr)))

(defn handle-update [uid entity params]
  (println (format "Received update request: uid: %s, entity: %s, params: %s" uid entity params))
  (let [params (walk/keywordize-keys (read-string params))]
    (assert (contains? @objs entity) (format "Cannot update unknown entity: %s, objs: %s" entity @objs))
    (assert (contains? (set (keys params)) :id) (format "Can only update entity %s by id. keys: %s" entity (keys params)))
    (modify @data-source entity params)))

(defn handle-delete [uid entity id]
  (println (format "Received delete request: uid: %s, entity: %s, id: %s" uid entity id))
  (delete @data-source entity id))
