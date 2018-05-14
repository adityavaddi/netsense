(ns appl.lighting-control.core
  (:require [neowrap.neowrapper :as neo4j]
            [clojure.data.json :as json]
            [clojure.java.io :as io]))

(defprotocol ILightingGroup
  "An additional protocol for lighting groups to declare the
  additional functionality beyond a organizational group."
  ;; TODO: Move to Organizational Groups.
  (get-nodes [this] "Return the nodes under this LG."))

(defrecord LightingGroup [groupid]
  ILightingGroup
  (get-nodes [_] (-> "cyphers/get_group.cypher"
                     io/resource
                     slurp
                     (neo4j/executeQuery {"groupid" groupid})
                     (json/read-str :key-fn keyword)
                     :group
                     :nodeList)))

(defprotocol LightingPolicy
  "A protocol, initially, to abstract commonalities between Schedules,
  DHProfiles, and PDProfiles when used by `cape/calculate-lg-update`."

  (neo4j-label [this] "Return the Label used in Neo4j for this object.")
  (props [this] "Return the `props` block used in a CASEL.")
  (to-group-name [this] "Return the CASEL OP for applying to a Lighting Group.")
  (to-group [this
             base-casel
             id
             nodes]
    "Apply this policy to a Lighting Group.")
  (to-nodes-name [this] "Return the CASEL OP for applying to Nodes.")
  (to-nodes [this
             base-casel
             id
             nodes]
    "Apply this policy to a collection of Nodes.")
  (delete-group [this] "Remove this policy from a Lighting Group.")
  )

(defprotocol OptionalLightingPolicy
  (delete-nodes-cypher [this] "Remove this policy from a collection of Nodes.")
  )

(defn to-group* [this
                 base-casel
                 id
                 nodes]
  (-> base-casel
      (assoc :type (to-group-name this)
             :groupprops {:groupids nodes})
      (merge ((props this) id))))

(defn to-nodes* [this
                 base-casel
                 id
                 nodes]
  (-> base-casel
      (assoc :type (to-nodes-name this)
             :nodeprops {:nodeids nodes})
      (merge ((props this) id))))

(defn delete-group* [this]
  (slurp (io/resource (str "cyphers/"
                            (.toLowerCase (neo4j-label this))
                            "_remove_group.cypher"))))

(defn delete-nodes-cypher* [this]
  (slurp (io/resource (str "cyphers/"
                            (.toLowerCase (neo4j-label this))
                            "_remove_nodes.cypher"))))

(defn props* [this]
  {:pre [(satisfies? LightingPolicy
                     this) ;; Assert we'll have `neo4j-label`
         ]}
  (let [base-name (.toLowerCase (neo4j-label this))
        pprops (keyword (str base-name "props"))
        pid (keyword (str base-name "id"))]
    #(do {pprops {pid %}})))

(defrecord Schedule [])

(extend Schedule
  LightingPolicy
  {:neo4j-label (fn [_] "Schedule")
   :props props*
   :to-group-name (fn [_] "applyScheduleToGroup")
   :to-group to-group*
   :to-nodes-name (fn [_] "applyScheduleToNodes")
   :to-nodes to-nodes*
   :delete-group delete-group*})

(defrecord ETDHProfile [])

(extend ETDHProfile
  LightingPolicy
  {:neo4j-label (fn [_] "ETDHProfile")
   :props props*
   :to-group-name (fn [_] "applyETDHtoGroup")
   :to-group to-group*
   :to-nodes-name (fn [_] "applyETDHtoNodes")
   :to-nodes to-nodes*
   :delete-group delete-group*}
  OptionalLightingPolicy
  {:delete-nodes-cypher delete-nodes-cypher*})

(defrecord DHProfile [])

(extend DHProfile
  LightingPolicy
  {:neo4j-label (fn [_] "DHProfile")
   :props props*
   :to-group-name (fn [_] "applyDHtoGroup")
   :to-group to-group*
   :to-nodes-name (fn [_] "applyDHtoNodes")
   :to-nodes to-nodes*
   :delete-group delete-group*}
  OptionalLightingPolicy
  {:delete-nodes-cypher delete-nodes-cypher*})

(defrecord PDProfile [])

(extend PDProfile
  LightingPolicy
  {:neo4j-label (fn [_] "PDProfile")
   :props props*
   :to-group-name (fn [_] "applyPDtoGroup")
   :to-group to-group*
   :to-nodes-name (fn [_] "applyPDtoNodes")
   :to-nodes to-nodes*
   :delete-group delete-group*}
  OptionalLightingPolicy
  {:delete-nodes-cypher delete-nodes-cypher*})



(defprotocol Overrides)

(extend-protocol Overrides
  DHProfile
  PDProfile)
