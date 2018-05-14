(ns service.stubbeddatasrc
  (:require [clojure.core.match :refer [match]]
            [common.core]
            [service.datasrc]))

(defn get-by-field [entities [fieldname fieldval]]
    (common.core/single-or-nil (filter #(= fieldval (.toString (get % (keyword fieldname)))) entities)))

(def StubbedDataSource
  (let [stub-data (atom (common.core/get-config "stubdata.edn"))]
    (reify service.datasrc/DataSource
      (service.datasrc/schema [_ _ _ _] nil)
      (service.datasrc/select [_ entity fltr]
        (let [entities (get @stub-data (service.datasrc/get-key entity))
          res (match [fltr]
            ["all()"] entities
            [_] (get-by-field entities (clojure.string/split fltr #"=")))] res))
      (service.datasrc/insert [_ entity params]
        (let [[_ _ _ required obj-key trimmed] (service.datasrc/get-descriptors entity params)
              required (map (comp keyword :name) required)
              entities (get @stub-data (service.datasrc/get-key entity))
              with-id (fn [entity entities] (into {:id (+ 1 (apply max 0 (map :id entities)))} entity))]
          (assert (every? #(contains? (set (keys params)) %) required)
            (format "Missing required param. Required: %s, Provided: %s" (apply str required) params))
          (let [res (with-id trimmed entities)]
            (swap! stub-data assoc obj-key (into [] (concat entities [res]))) res)))
      (service.datasrc/modify [this entity params]
        (let [[_ _ _ _ obj-key trimmed] (service.datasrc/get-descriptors entity params)
              entities (get @stub-data (service.datasrc/get-key entity))
              obj-id (str (get params :id))
              previous (service.datasrc/select this entity (format "id=%s" obj-id))]
          (assert previous (format "Cannot update non-existent entity: %s, id: %s" entity obj-id))
          (let [res (merge previous (dissoc trimmed :id))]
          (swap! stub-data assoc obj-key
            (into [] (concat (remove #(= (str (get % :id)) obj-id) entities) [res]))) res)))
      (service.datasrc/delete [this entity id]
        (let [res (service.datasrc/select this entity (format "id=%s" id))
              obj-key (service.datasrc/get-key entity)]
          (assert res (format "Cannot delete non-existent entity: %s, id: %s" entity id))
          (swap! stub-data assoc obj-key (into [] (remove #(= (str (get % :id)) (str id)) (get @stub-data obj-key)))) res)))))
