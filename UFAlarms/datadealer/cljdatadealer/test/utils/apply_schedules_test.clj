(ns utils.apply-schedules-test
  (:require [clojure.test :refer :all]
            [dealer.devsvcctrl-test :as dev-ctrl-test]
            [mqtt.fixture :as mqtt]
            [kafka.mock :as kafka]
            [utils
             [cape-test :refer [run]]
             [cassandra_fixture :as cass]
             [neo4j-fixture :as neo4j-fixture]
             [schedules :as schedules]]))

(use-fixtures :once (join-fixtures [cass/cassandra-fixture
                                    neo4j-fixture/neo4j-fixture
                                    kafka/kafka-fixture
                                    mqtt/mqtt-fixture
                                    dev-ctrl-test/device-service-control-fixture
                                    utils.cape-test/mock-google-api]))

(def orgid "schedules-testorg")
(def siteid "schedules-testsite")
(def group1 "schedules-group1")
(def group2 "schedules-group2")

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
                  (is (:success (run (update node
                                             :nodeprops
                                             merge {:name (str "node" ii)
                                                    :nodeid (str "node" ii)})))))
                nodeids))))

(def no-network-schedule
  {:highTime "01:02:03"
   :highLevel 45
   :lowTime "06:07:08"
   :lowLevel 90
   :photocell_enabled false
   :photocell_highLevel 100
   :photocell_lowLevel 0})

(defn create-schedules [scheduleids]
  (let [schedule {:type "createSchedule"
                  :user      "uberuser"
                  :orgprops  {:orgid orgid}
                  :siteprops {:siteid siteid}
                  :scheduleprops {:scheduleid "schedule1"
                                  :name "Test Schedule 1"
                                  :description "This is a test schedule"
                                  :events [{:days ["mon" "tue" "wed" "thu" "fri" "sat" "sun"]
                                            :actions [{:time "sunset"     :level 100}
                                                      {:time "sunrise" :level 0}]}]
                                  :network no-network-schedule}}]
    (doall (map (fn [ii]
                  (is (:success (run (update schedule
                                             :scheduleprops
                                             merge {:name (str "schedule" ii)
                                                    :scheduleid (str "schedule" ii)})))))
                scheduleids))))

(defn get-node [nodeid]
  (let [node {:type "getNode"
              :user "uberuser"
              :orgprops  {:orgid orgid}
              :siteprops {:siteid siteid}
              :nodeprops {:nodeid nodeid}}
        received (run node)]
    (is (:success received))
    (:node received)))

(defn get-schedule [scheduleid]
  (let [schedule {:type "getSchedule"
                  :user "uberuser"
                  :orgprops  {:orgid orgid}
                  :siteprops {:siteid siteid}
                  :scheduleprops {:scheduleid scheduleid}}
        received (run schedule)]
    (is (:success received))
    (:schedule received)))

(defn get-all-schedules []
  (let [schedules {:type "getAllSchedules"
                   :user "uberuser"
                   :orgprops  {:orgid orgid}
                   :siteprops {:siteid siteid}}
        received (run schedules)]
    (is (:success received))
    (:items received)))

(defn get-all-groups []
  (let [groups {:type "getAllGroups"
                :user "uberuser"
                :orgprops  {:orgid orgid}
                :siteprops {:siteid siteid}}
        received (run groups)]
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

(defn make-group [groupid nodeids]
  (let [group {:type "createGroup"
               :user "uberuser"
               :orgprops  {:orgid orgid}
               :siteprops {:siteid siteid}
               :groupprops {:groupid groupid
                            :description groupid
                            :nodeList nodeids
                            :name groupid
                            :type "lighting"}}
        received (run group)]
    (is (nil? (get received :exception)))
    (is (:success received))
    (:group received)))

(defn update-group [groupid nodeids]
  (let [groupprops (-> {:type "getGroup"
                        :user "uberuser"
                        :orgprops  {:orgid orgid}
                        :siteprops {:siteid siteid}
                        :groupprops {:groupid groupid}}
                       run
                       (doto (-> :success
                                 true?
                                 is))
                       :group
                       (assoc :nodeList nodeids))
        group {:type "updateGroup"
               :user "uberuser"
               :orgprops  {:orgid orgid}
               :siteprops {:siteid siteid}
               :groupprops groupprops}
        received (run group)]
    (is (nil? (get received :exception)))
    (is (:success received))
    (:group received)))

(defn rename-schedule [scheduleid name]
  (let [scheduleprops (-> {:type "getSchedule"
                           :user "uberuser"
                           :orgprops  {:orgid orgid}
                           :siteprops {:siteid siteid}
                           :scheduleprops {:scheduleid scheduleid}}
                          run
                          (doto (-> :success
                                    true?
                                    is))
                          :schedule
                          (assoc :name name))
        schedule {:type "updateSchedule"
                  :user "uberuser"
                  :orgprops  {:orgid orgid}
                  :siteprops {:siteid siteid}
                  :scheduleprops scheduleprops}
        gds schedules/get-daily-schedule
        off-network (atom :uncalled)
        received (with-redefs [schedules/get-daily-schedule (fn [& [{:keys [network]} & _ :as args]]
                                                              (reset! off-network network)
                                                              (apply gds args))]
                   (run schedule))]
    (is (or (= no-network-schedule
               @off-network)
            (= :uncalled
               @off-network)))
    (is (nil? (get received :exception)))
    (is (:success received))
    (:schedule received)))

(defn apply-schedule [type scheduleid & [groupid nodeid nodeids]]
  (let [casel {:type          type
               :user          "uberuser"
               :orgprops      {:orgid orgid}
               :siteprops     {:siteid siteid}
               :groupprops    {:groupids [groupid]}
               :nodeprops     {:type "LightingScheduledEvent"
                               :nodeid nodeid
                               :nodeids nodeids}
               :scheduleprops {:scheduleid scheduleid :nodeids nodeids}}
        gds schedules/get-daily-schedule
        off-network (atom :uncalled)]
    (is (true? (with-redefs [schedules/get-daily-schedule (fn [& [{:keys [network]} & _ :as args]]
                                                            (reset! off-network network)
                                                            (apply gds args))]
                 (-> (run casel)
                     :success))))
    (is (or (= no-network-schedule
               @off-network)
            (= :uncalled
               @off-network)))))

(defn node-resp [nodeids]
  (map #(do {:name % :nodeid %}) nodeids))

(deftest apply-schedules-test
  (let [orgPropsCreate {:type     "createOrg"
                        :user     "uberuser"
                        :orgprops {:po          "uberorg"
                                   :orgid       orgid
                                   :name        "uberorg"
                                   :street1     "street1"
                                   :street2     "street2"
                                   :city        "city"
                                   :state       "state"
                                   :postal_code "postal_code"
                                   :country     "country"
                                   :contact     "contact"
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
                                     :time_zone "America/Los_Angeles"}}
        orgCreated (run orgPropsCreate)
        siteCreated (run sitePropsCreate)]

    (is (:success orgCreated))
    (is (:success siteCreated))

    (create-schedules (range 1 4))
    (create-nodes (range 1 6))

    (apply-schedule "applyScheduleToSite" "schedule1")
    (is (= (:scheduleid (get-node "node1")) "schedule1"))
    (is (= (:scheduleid (get-node "node2")) "schedule1"))
    (is (= (:scheduleid (get-node "node3")) "schedule1"))
    (is (= (:scheduleid (get-node "node4")) "schedule1"))
    (is (= (:scheduleid (get-node "node5")) "schedule1"))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule1"))) (node-resp ["node1" "node2" "node3" "node4" "node5"])))

    (rename-schedule "schedule1" "schedule1.0")

    (make-group group1 ["node1" "node2"])
    (apply-schedule "applyScheduleToGroup" "schedule2" group1)

    (is (= (:scheduleid (get-node "node1")) "schedule2"))
    (is (= (:scheduleid (get-node "node2")) "schedule2"))
    (is (= (:scheduleid (get-node "node3")) "schedule1"))
    (is (= (:scheduleid (get-node "node4")) "schedule1"))
    (is (= (:scheduleid (get-node "node5")) "schedule1"))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule1"))) (node-resp ["node3" "node4" "node5"])))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule2"))) (node-resp ["node1" "node2"])))
    (is (= (:nodeList (get-group group1)) ["node1" "node2"]))

    (apply-schedule "applyScheduleToNode" "schedule2" nil "node5")

    (is (= (:scheduleid (get-node "node1")) "schedule2"))
    (is (= (:scheduleid (get-node "node2")) "schedule2"))
    (is (= (:scheduleid (get-node "node3")) "schedule1"))
    (is (= (:scheduleid (get-node "node4")) "schedule1"))
    (is (= (:scheduleid (get-node "node5")) "schedule2"))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule1"))) (node-resp ["node3" "node4"])))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule2"))) (node-resp ["node1" "node2" "node5"])))

    (update-group group1 ["node1" "node2" "node3"])
    (is (= (:nodeList (get-group group1)) ["node1" "node2" "node3"]))

    (is (= (:scheduleid (get-node "node1")) "schedule2"))
    (is (= (:scheduleid (get-node "node2")) "schedule2"))
    (is (= (:scheduleid (get-node "node3")) "schedule2"))
    (is (= (:scheduleid (get-node "node4")) "schedule1"))
    (is (= (:scheduleid (get-node "node5")) "schedule2"))

    (create-nodes (range 10 15))
    (make-group group2 ["node10" "node11" "node12" "node13" "node14"])
    (apply-schedule "applyScheduleToNodes" "schedule3" nil nil ["node10" "node11" "node12" "node13" "node14"])

    (is (= (:scheduleid (get-node "node10")) "schedule3"))
    (is (= (:scheduleid (get-node "node11")) "schedule3"))
    (is (= (:scheduleid (get-node "node12")) "schedule3"))
    (is (= (:scheduleid (get-node "node13")) "schedule3"))
    (is (= (:scheduleid (get-node "node14")) "schedule3"))

                                        ;(is (= (sort-by :nodeid (:nodes (get-schedule "default"))) []))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule1"))) (node-resp ["node4"])))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule2"))) (node-resp ["node1" "node2" "node3" "node5"])))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule3"))) (node-resp ["node10" "node11" "node12" "node13" "node14"])))

    (apply-schedule "applyScheduleToGroup" "schedule1" group2)
    (is (= (:nodeList (get-group group1)) ["node1" "node2" "node3"]))
    (is (= (:nodeList (get-group group2)) ["node10" "node11" "node12" "node13" "node14"]))

    (is (= (:scheduleid (get-node "node1")) "schedule2"))
    (is (= (:scheduleid (get-node "node2")) "schedule2"))
    (is (= (:scheduleid (get-node "node3")) "schedule2"))
    (is (= (:scheduleid (get-node "node4")) "schedule1"))
    (is (= (:scheduleid (get-node "node5")) "schedule2"))

    (is (= (:scheduleid (get-node "node10")) "schedule1"))
    (is (= (:scheduleid (get-node "node11")) "schedule1"))
    (is (= (:scheduleid (get-node "node12")) "schedule1"))
    (is (= (:scheduleid (get-node "node13")) "schedule1"))
    (is (= (:scheduleid (get-node "node14")) "schedule1"))

                                        ;(is (= (sort-by :nodeid (:nodes (get-schedule "default"))) []))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule1"))) (node-resp ["node10" "node11" "node12" "node13" "node14" "node4"])))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule2"))) (node-resp ["node1" "node2" "node3" "node5"])))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule3"))) []))

    (update-group group2 ["node10" "node1"])

    (is (= (:scheduleid (get-node "node1")) "schedule1"))
    (is (= (:scheduleid (get-node "node2")) "schedule2"))
    (is (= (:scheduleid (get-node "node3")) "schedule2"))
    (is (= (:scheduleid (get-node "node4")) "schedule1"))
    (is (= (:scheduleid (get-node "node5")) "schedule2"))

    (is (= (:scheduleid (get-node "node10")) "schedule1"))
    (is (= (:scheduleid (get-node "node11")) "schedule1"))
    (is (= (:scheduleid (get-node "node12")) "schedule1"))
    (is (= (:scheduleid (get-node "node13")) "schedule1"))
    (is (= (:scheduleid (get-node "node14")) "schedule1"))

                                        ;(is (= (sort-by :nodeid (:nodes (get-schedule "default"))) (node-resp ["node11" "node12" "node13" "node14"])))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule1"))) (node-resp ["node1" "node10" "node11" "node12" "node13" "node14" "node4"])))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule2"))) (node-resp ["node2" "node3" "node5"])))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule3"))) []))

    (is (= (:nodeList (get-group group1)) ["node2" "node3"]))
    (is (= (:nodeList (get-group group2)) ["node1" "node10"]))

    (add-node-to-group group1 "node1")

    (is (= (:scheduleid (get-node "node1")) "schedule2"))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule1"))) (node-resp ["node10" "node11" "node12" "node13" "node14" "node4"])))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule2"))) (node-resp ["node1" "node2" "node3" "node5"])))
    (is (= (:nodeList (get-group group1)) ["node1" "node2" "node3"]))
    (is (= (:nodeList (get-group group2)) ["node10"]))

    (remove-node-from-group group1 "node3")

    (is (= (:scheduleid (get-node "node3")) "schedule1"))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule1"))) (node-resp ["node10" "node11" "node12" "node13" "node14" "node3" "node4"])))
    (is (= (sort-by :nodeid (:nodes (get-schedule "schedule2"))) (node-resp ["node1" "node2" "node5"])))
    (is (= (:nodeList (get-group group1)) ["node1" "node2"]))
    (is (= (:nodeList (get-group group2)) ["node10"]))

    (apply-schedule "applyScheduleToSite" "schedule2")

    (let [all-schedules (get-all-schedules)
          schedule1 (into {} (filter #(= "schedule1" (:scheduleid %)) all-schedules))
          schedule2 (into {} (filter #(= "schedule2" (:scheduleid %)) all-schedules))
          schedule3 (into {} (filter #(= "schedule3" (:scheduleid %)) all-schedules))
          slgid (->> {:type "getAllGroups"
                      :user "uberuser"
                      :orgprops {:orgid orgid}
                      :siteprops {:siteid siteid}}
                     run
                     :items
                     (filter (comp #{"Site Lighting Group"}
                                   :name))
                     (map :groupid)
                     first)]

      (is (= (set (:sites schedule1)) #{}))
      (is (= (set (:groups schedule1)) #{{:name group2 :groupid group2}}))

      (is (= (set (:sites schedule2)) #{{:name "Site Lighting Group", :groupid slgid}}))
      (is (= (set (:groups schedule2)) #{{:name group1 :groupid group1}}))

      (is (= (set (:sites schedule3)) #{}))
      (is (= (set (:groups schedule3)) #{})))

    (let [all-groups (get-all-groups)
          g1 (into {} (filter #(= group1 (:groupid %)) all-groups))
          g2 (into {} (filter #(= group2 (:groupid %)) all-groups))]

      (is (= (set (:schedules  g1)) #{{:name "schedule2", :scheduleid "schedule2"}}))
      (is (= (set (:dhprofiles g1)) #{}))
      (is (= (set (:pdprofiles g1)) #{}))

      (is (= (set (:schedules  g2)) #{{:name "schedule1.0", :scheduleid "schedule1"}}))
      (is (= (set (:dhprofiles g2)) #{}))
      (is (= (set (:pdprofiles g2)) #{})))))
