(ns utils.apply-profiles-test
  (:require [clojure.test :refer :all]
            [dealer.devsvcctrl-test :as dctrl-test]
            [mqtt.fixture :as mqtt]
            [kafka.mock :as kafka]
            [utils
             [cape-test :refer [run]]
             [cassandra_fixture :as cass]
             [neo4j-fixture :as neo4j-fixture]]))

(use-fixtures :once (join-fixtures [cass/cassandra-fixture
                                    neo4j-fixture/neo4j-fixture
                                    mqtt/mqtt-fixture
                                    kafka/kafka-fixture
                                    dctrl-test/device-service-control-fixture
                                    utils.cape-test/mock-google-api]))

(def orgid "profiles-testorg")
(def siteid "profiles-testsite")
(def group1 "profiles-group1")
(def group2 "profiles-group2")

(defn create-nodes [nodeids]
  (let [node {:type "createNode"
              :user "uberuser"
              :orgprops  {:orgid orgid}
              :siteprops {:siteid siteid}
              :nodeprops {:nodeid    "node"
                          :name      "Test Node"
                          :model     "unode-v4"
                          :latitude  "37.383712"
                          :longitude "-121.989921"}}]
    (doall (map (fn [ii]
                  (is (:success (run (-> node
                                         (assoc-in [:nodeprops :nodeid] (str "pnode" ii))
                                         (assoc-in [:nodeprops :name] (str "pnode" ii)))
                                  ))))
                nodeids))))

(defn create-dhprofiles [dhprofileids]
  (let [dhprofile {:setPoint "100"
                   :gain "100"
                   :resetTime "8"
                   :minDrive "1"
                   :maxDrive "100"
                   :slewRate "0.05"}
        base-dhprofile {:type "createDHProfile"
                        :user      "uberuser"
                        :orgprops  {:orgid orgid}
                        :siteprops {:siteid siteid}
                        :nodeprops {:type "DaylightHarvesting"}
                        :dhprofileprops dhprofile}]
    (doall (map (fn [ii]
                  (is (:success (run (-> base-dhprofile
                                         (assoc-in [:dhprofileprops :dhprofileid] (str "dhprofile" ii))
                                         (assoc-in [:dhprofileprops :name] (str "dhprofile" ii)))))))
                dhprofileids))))

(defn create-pdprofiles [pdprofileids]
  (let [pdprofile {:minLevel "23"
                   :maxLevel "79"
                   :beginTime "00:00:00"
                   :endTime "23:59:59"
                   :radius "15"
                   :detection_duration "12"}
        base-pdprofile {:type "createPDProfile"
                        :user      "uberuser"
                        :orgprops  {:orgid orgid}
                        :siteprops {:siteid siteid}
                        :nodeprops {:type "ProximityDimming"}
                        :pdprofileprops pdprofile}]
    (doall (map (fn [ii]
                  (is (:success (run (-> base-pdprofile
                                         (assoc-in  [:pdprofileprops :pdprofileid] (str "pdprofile" ii))
                                         (assoc-in [:pdprofileprops :name] (str "pdprofile" ii)))))))
                pdprofileids))))


(defn get-node [nodeid]
  (let [node {:type "getNode"
              :user "uberuser"
              :orgprops  {:orgid orgid}
              :siteprops {:siteid siteid}
              :nodeprops {:nodeid nodeid}}
        received (run node)]
    (is (:success received))
    (:node received)))

(defn get-nodes []
  (let [node {:type "getAllNodesForSite"
              :user "uberuser"
              :orgprops  {:orgid orgid}
              :siteprops {:siteid siteid}}
        received (run node)]
    (is (:success received))
    (:items received)))

(defn get-dhprofiles []
  (let [dhprofile {:type "getAllDHProfiles"
                   :user "uberuser"
                   :orgprops  {:orgid orgid}
                   :siteprops {:siteid siteid}}
        received (run dhprofile)]
    (is (:success received))
    (:items received)))

(defn get-pdprofiles []
  (let [pdprofile {:type "getAllPDProfiles"
                   :user "uberuser"
                   :orgprops  {:orgid orgid}
                   :siteprops {:siteid siteid}}
        received (run pdprofile)]
    (is (:success received))
    (:items received)))

(defn get-group [groupid]
  (let [group {:type "getGroup"
               :user "uberuser"
               :orgprops  {:orgid orgid}
               :siteprops {:siteid siteid}
               :groupprops {:groupid groupid}}
        received (run group)]
    (is (:success received))
    (:group received)))

(defn get-all-groups []
  (let [groups {:type "getAllGroups"
                :user "uberuser"
                :orgprops  {:orgid orgid}
                :siteprops {:siteid siteid}}
        received (run groups)]
    (is (:success received))
    (:items received)))

(defn add-node-to-group [groupid nodeid]
  (let [group {:type "addNodeToGroup"
               :user "uberuser"
               :orgprops  {:orgid orgid}
               :siteprops {:siteid siteid}
               :groupprops {:groupid groupid}
               :nodeprops {:nodeid nodeid}}
        received (run group)]
    (is (:success received))))

(defn remove-node-from-group [groupid nodeid]
  (let [group {:type "removeNodeFromGroup"
               :user "uberuser"
               :orgprops  {:orgid orgid}
               :siteprops {:siteid siteid}
               :groupprops {:groupid groupid}
               :nodeprops {:nodeid nodeid}}
        received (run group)]
    (is (:success received))))

(defn make-group [type groupid nodeids]
  (let [group {:type type
               :user "uberuser"
               :orgprops  {:orgid orgid}
               :siteprops {:siteid siteid}
               :groupprops {:groupid groupid
                            :description groupid
                            :nodeList nodeids
                            :name groupid
                            :type "lighting"}}
        received (run group)]
    (is (:success received))
    (:group received)))

(defn apply-pdprofile [type pdprofileid & [groupid nodeid]]
  (let [apply {:type          type
               :user          "uberuser"
               :orgprops      {:orgid orgid}
               :siteprops     {:siteid siteid}
               :groupprops    {:groupids [groupid]}
               :nodeprops     {:type "ProximityDimming" :nodeids [nodeid]}
               :pdprofileprops {:pdprofileid pdprofileid}}
        applied (run apply)]
    (is (:success applied))))

(defn apply-dhprofile [type dhprofileid & [groupid nodeid]]
  (let [apply {:type          type
               :user          "uberuser"
               :orgprops      {:orgid orgid}
               :siteprops     {:siteid siteid}
               :groupprops    {:groupids [groupid]}
               :nodeprops     {:type "DaylightHarvesting" :nodeids [nodeid]}
               :dhprofileprops {:dhprofileid dhprofileid}}
        applied (run apply)]
    (is (:success applied))))

(defn node-resp [nodeids]
  (map #(do {:nodename % :nodeid %}) nodeids))

(deftest apply-profiles-test
  (let [orgPropsCreate {:type     "createOrg"
                        :user     "uberuser"
                        :orgprops {:po           "uberorg"
                                   :orgid        orgid
                                   :name         "uberorg"
                                   :street1      "street1"
                                   :street2      "street2"
                                   :city         "city"
                                   :state        "state"
                                   :postal_code  "postal_code"
                                   :country      "country"
                                   :contact      "contact"
                                   :contact_name "contact_name"}}
        sitePropsCreate {:type      "createSite"
                         :user      "uberuser"
                         :orgprops  {:orgid orgid}
                         :siteprops {:siteid      siteid
                                     :name        "Test site"
                                     :street1     "480 Oakmead Pkwy"
                                     :street2     "Suite 200"
                                     :city        "Sunnyvale"
                                     :state       "CA"
                                     :postal_code "94085"
                                     :country     "USA"
                                     :latitude    "37.383712"
                                     :longitude   "-121.989921"
                                     :altitude    "125"
                                     :country_code "US"
                                     :time_zone   "America/Los_Angeles"
                                     }}
        orgCreated (run orgPropsCreate)
        siteCreated (run sitePropsCreate)]

    (is (:success orgCreated))
    (is (:success siteCreated))

    (create-pdprofiles (range 1 3))
    (create-dhprofiles (range 1 3))
    (create-nodes (range 1 11))

    (make-group "createGroup" group1 ["pnode1" "pnode2" "pnode3" "pnode4" "pnode5"])
    (make-group "createGroup" group2 ["pnode6" "pnode7" "pnode8" "pnode9" "pnode10"])

    (apply-pdprofile "applyPDtoGroup" "pdprofile1" group1)
    (apply-dhprofile "applyDHtoGroup" "dhprofile1" group2)

                                        ;(println (get-nodes))


    (is (= (:pdprofileid (get-node "pnode1")) "pdprofile1"))
    (is (= (:pdprofileid (get-node "pnode2")) "pdprofile1"))
    (is (= (:pdprofileid (get-node "pnode3")) "pdprofile1"))
    (is (= (:pdprofileid (get-node "pnode4")) "pdprofile1"))
    (is (= (:pdprofileid (get-node "pnode5")) "pdprofile1"))

    (is (= (:dhprofileid (get-node "pnode6")) "dhprofile1"))
    (is (= (:dhprofileid (get-node "pnode7")) "dhprofile1"))
    (is (= (:dhprofileid (get-node "pnode8")) "dhprofile1"))
    (is (= (:dhprofileid (get-node "pnode9")) "dhprofile1"))
    (is (= (:dhprofileid (get-node "pnode10")) "dhprofile1"))

    (apply-pdprofile "applyPDtoGroup" "pdprofile2" group1)
    (apply-dhprofile "applyDHtoGroup" "dhprofile2" group2)

    (is (= (:pdprofileid (get-node "pnode1")) "pdprofile2"))
    (is (= (:pdprofileid (get-node "pnode2")) "pdprofile2"))
    (is (= (:pdprofileid (get-node "pnode3")) "pdprofile2"))
    (is (= (:pdprofileid (get-node "pnode4")) "pdprofile2"))
    (is (= (:pdprofileid (get-node "pnode5")) "pdprofile2"))

    (is (= (:dhprofileid (get-node "pnode6")) "dhprofile2"))
    (is (= (:dhprofileid (get-node "pnode7")) "dhprofile2"))
    (is (= (:dhprofileid (get-node "pnode8")) "dhprofile2"))
    (is (= (:dhprofileid (get-node "pnode9")) "dhprofile2"))
    (is (= (:dhprofileid (get-node "pnode10")) "dhprofile2"))

    (is (= [[] [{:groupid "profiles-group2" :name "profiles-group2"}]]
           (->> (get-dhprofiles)
                (sort-by :dhprofileid)
                (map :groups))))
    (is (= [[] [{:groupid "profiles-group1" :name "profiles-group1"}]]
           (->> (get-pdprofiles)
                (sort-by :pdprofileid)
                (map :groups))))

    (let [all-groups (get-all-groups)
          g1 (into {} (filter #(= group1 (:groupid %)) all-groups))
          g2 (into {} (filter #(= group2 (:groupid %)) all-groups))]

      (is (= (set (:schedules  g1)) #{}))
      (is (= (set (:dhprofiles g1)) #{}))
      (is (= (set (:pdprofiles g1)) #{{:pdprofileid "pdprofile2" :name "pdprofile2"}}))

      (is (= (set (:schedules  g2)) #{}))
      (is (= (set (:dhprofiles g2)) #{{:dhprofileid "dhprofile2" :name "dhprofile2"}}))
      (is (= (set (:pdprofiles g2)) #{})))
    ))
