(ns utils.cape-test
  (:require [appl.proximity-dimming :as pd]
            [clojure
             [test :refer :all]
             [walk :refer :all]]
            [clostache.parser :refer [render-resource]]
            [clojure.data.json :as json]
            [clojure.tools.logging :refer :all]
            [dealer
             [devsvcctrl :as dctrl]
             [devsvcctrl-test :as dctrl-test]]
            [logging.activitylogger :as al]
            [mqtt.fixture :as mqtt]
            [kafka.mock :as kafka]
            [neowrap.neowrapper :as neo4j]
            [kafka.mock :as kafka]
            [utils
             [cape :as cape]
             [cassandra_fixture :as cass]
             [neo4j-fixture :as neo4j-fixture]
             [nodeconfig :as nodeconfig]
             [schedules :as sch]])
  (:import java.util.concurrent.Executors))

(defn scoped-thread-pool-fixture
  [f]
  (with-redefs [cape/cape-thread-pool (Executors/newFixedThreadPool 40)]
    (try
      (f)
      (finally
        (.shutdown cape/cape-thread-pool)))))

(defn mock-google-api
  [f]
  (with-redefs [cape/update-country-code (fn [props lat lon]
                                           (assoc props "country_code" "US"))]
    (f)))

(use-fixtures :once (join-fixtures [cass/cassandra-fixture
                                    neo4j-fixture/neo4j-fixture
                                    mqtt/mqtt-fixture
                                    kafka/kafka-fixture
                                    dctrl-test/device-service-control-fixture
                                    dctrl-test/lfs-timeout-fixture
                                    scoped-thread-pool-fixture
                                    mock-google-api]))

(defn vectors-to-sets
  [form]
  (clojure.walk/prewalk (fn [form]
                          (cond
                            (instance? clojure.lang.IMapEntry form) form
                            (vector? form) (set form)
                            :else form))
                        form))

(def run
  (comp keywordize-keys
        json/read-str
        cape/executer-main
        stringify-keys))

(defn uuid []
  (.. java.util.UUID
      randomUUID
      toString))

(def ^:dynamic *user* "uberuser")

(defn create-test-org
  [& [{:as orgprops}]]
  (let [{:keys [orgid]
         :as orgprops} (merge {:orgid (uuid)
                               :po "uberorg"
                               :name "Test Org"}
                              orgprops)]
    (assert (true? (-> {:type "createOrg"
                        :user *user*
                        :orgprops orgprops}
                   run
                   :success)))
    (neo4j/executeQuery (str "MATCH (o:Org {orgid: \"" orgid "\"}) SET o:TESTONLY"))
    orgid))

(def default-test-site
  {:name "Test Site"
   :latitude "37.380996"
   :longitude "-121.992299"})

(defn create-test-site
  [orgid & [{:as siteprops}]]
  (let [uuid (uuid)
        siteprops (merge
                   (assoc default-test-site
                          :siteid uuid)
                   siteprops)]
    (is (true? (-> {:type "createSite"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops siteprops}
                   run
                   :success)))
    (neo4j/executeQuery (str "MATCH (s:Site {siteid: \"" uuid "\"}) SET s:TESTONLY"))
    uuid))

(def default-test-node
  {:latitude "37.380996"
   :longitude "-121.992299"
   :model "unode-v4"})

(defn create-test-node
  [orgid siteid & [{:as nodeprops}]]
  (let [{:keys [nodeid]
         :as nodeprops} (merge (assoc default-test-node
                                      :nodeid (uuid)
                                      :model "unode-v4")
                               nodeprops)]
    (is (true? (-> {:type "createNode"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :nodeprops nodeprops}
                   run
                   :success)))
    (neo4j/executeQuery (str "MATCH (n:Node {nodeid: \"" nodeid "\"}) SET n:TESTONLY"))
    nodeid))

(defn create-test-node-empty
  [& [{:as nodeprops}]]
  (let [{:keys [nodeid]
         :as nodeprops} (merge {:nodeid (uuid)
                                :model "test"}
                               nodeprops)]
    (is (true? (-> {:type "createEmptyNode"
                    :user *user*
                    :nodeprops nodeprops}
                   run
                   :success)))
    (neo4j/executeQuery (str "MATCH (n:Node {nodeid: \"" nodeid "\"}) SET n:TESTONLY"))
    nodeid))

(def default-test-no-network-schedule
  {:highTime "17:00:00"
   :highLevel 100
   :lowTime "9:00:00"
   :lowLevel 0
   :photocell_enabled true
   :photocell_highLevel 100
   :photocell_lowLevel 0})

(def default-test-schedule
  {:name "Test Schedule"
   :description "This is a test schedule"
   :events [{:days ["sat" "sun"]
             :photocell_enabled false
             :photocell_highLevel 100
             :photocell_lowLevel 0
             :actions [{:time "sunset"     :level 100}
                       {:time "sunrise+30" :level 0}
                       {:time "23:00:00"   :level 0}
                       {:time "05:00:00"   :level 100}]}
            {:days ["mon" "tue" "wed" "thu" "fri"]
             :photocell_enabled false
             :photocell_highLevel 100
             :photocell_lowLevel 0
             :actions [{:time "sunset"     :level 100}
                       {:time "sunrise+30" :level 0}
                       {:time "23:00:00"   :level 50}
                       {:time "05:00:00"   :level 100}
                       {:time "sunrise"    :level 50}
                       {:time "sunset-30"  :level 50}]}]
   :network default-test-no-network-schedule})

(defn create-test-schedule
  [orgid siteid & [{:as scheduleprops}]]
  (let [uuid (uuid)
        {:keys [scheduleid]
         :as scheduleprops} (merge (assoc default-test-schedule
                                     :scheduleid uuid)
                                   scheduleprops)]
    (is (true? (-> {:type "createSchedule"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :scheduleprops (spy :debug scheduleprops)}
                   run
                   :success)))
    (neo4j/executeQuery (str "MATCH (s:Schedule {scheduleid: \"" scheduleid "\"})--(ce:CalendarEvents) SET s:TESTONLY, ce:TESTONLY"))
    scheduleid))

(defn create-test-group
  [orgid siteid & [{:as groupprops}]]
  {:pre [(or (nil? groupprops)
             (map? groupprops))]}
  (let [uuid (uuid)
        {:keys [groupid]
         :as groupprops} (merge {:groupid uuid
                                 :name "Test Group"
                                 :nodeList []
                                 :type "organizational"}
                                groupprops)]
    (is (true? (-> {:type "createGroup"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :groupprops groupprops}
                   run
                   :success)))
    (neo4j/executeQuery (str "MATCH (g:Group {groupid: \"" groupid "\"}) SET g:TESTONLY"))
    groupid))

(defn create-test-lighting-group
  [orgid siteid & [{:as groupprops}]]
  (let [uuid (uuid)
        {:keys [groupid]
         :as groupprops} (merge {:groupid uuid
                                 :name "Test Lighting Group"
                                 :nodeList []
                                 :type "lighting"}
                                groupprops)]
    (is (true? (-> {:type "createGroup"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :groupprops groupprops}
                   run
                   :success)))
    (neo4j/executeQuery (str "MATCH (g:Group {groupid: \"" groupid "\"}) SET g:TESTONLY"))
    groupid))

(def default-test-etdhprofile
  {:name "Example ETDHProfile"
   :high-lux 100
   :low-lux 50
   :low-driver 20
   :min-driver 80})

(defn create-test-etdhprofile
  [orgid siteid & [{:as etdhprofileprops}]]
  (let [uuid (uuid)
        {:keys [etdhprofileid]
         :as etdhprofileprops} (merge (assoc default-test-etdhprofile
                                             :etdhprofileid uuid)
                                      etdhprofileprops)]
    (is (true? (-> {:type "createETDHProfile"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :etdhprofileprops etdhprofileprops}
                   run
                   :success)))
    (neo4j/executeQuery (str "MATCH (p:ETDHProfile {etdhprofileid: \"" etdhprofileid "\"}) SET p:TESTONLY"))
    etdhprofileid))

(def default-test-dhprofile
  {:setPoint 100
   :gain 0.5
   :resetTime 8
   :minDrive 1
   :maxDrive 83
   :slewRate 50
   :beginTime "00:00:00"
   :endTime "23:59:59"
   :autocalibrateoptout false})

(defn create-test-dhprofile
  [orgid siteid & [{:as dhprofileprops}]]
  (let [uuid (uuid)
        {:keys [dhprofileid]
         :as dhprofileprops} (merge (assoc default-test-dhprofile
                                           :dhprofileid uuid)
                                    dhprofileprops)]
    (is (true? (-> {:type "createDHProfile"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :dhprofileprops dhprofileprops}
                   run
                   :success)))
    (neo4j/executeQuery (str "MATCH (p:DHProfile {dhprofileid: \"" dhprofileid "\"}) SET p:TESTONLY"))
    dhprofileid))

(def default-test-pdprofile
  {:minLevel 23
   :maxLevel 79
   :beginTime "00:00:00"
   :endTime "23:59:59"
   :mode "no-radius"
   :detection_duration 2})

(defn create-test-pdprofile
  [orgid siteid & [{:as pdprofileprops}]]
  (let [uuid (uuid)
        {:keys [pdprofileid]
         :as pdprofileprops} (merge (assoc default-test-pdprofile
                                           :pdprofileid uuid)
                                    pdprofileprops)]
    (is (true? (-> {:type "createPDProfile"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :pdprofileprops pdprofileprops}
                   run
                   :success)))
    (neo4j/executeQuery (str "MATCH (p:PDProfile {pdprofileid: \"" pdprofileid "\"}) SET p:TESTONLY"))
    pdprofileid))

(deftest main-test
  (let [orgPropsCreate {:type     "createOrg"
                        :user     "uberuser"
                        :orgprops {:po          "uberorg"
                                   :pname       "Uber Org"
                                   :orgid       "testorg"
                                   :type        "default"
                                   :name        "Test Org"
                                   :street1     "street1"
                                   :street2     "street2"
                                   :city        "city"
                                   :state       "state"
                                   :postal_code "postal_code"
                                   :country     "country"
                                   :contact_phone "contact_phone"
                                   :contact_name "contact_name"
                                   :contact_email "contact_email"}}
        orgPropsGet {:type     "getOrg"
                     :user     "uberuser"
                     :orgprops {:orgid "testorg"}}
        orgPropsUpdate {:type     "updateOrg"
                        :user     "uberuser"
                        :orgprops {:po          "uberorg"
                                   :orgid       "testorg"
                                   :type        "default"
                                   :name        "Test Org Updated"
                                   :street1     "123 abc st"
                                   :street2     "suite 123"
                                   :city        "some where"
                                   :state       "some state"
                                   :postal_code "code"
                                   :country     "some country"
                                   :contact_phone "some_phone"
                                   :contact_name "some_contact_name"
                                   :contact_email "some_contact_email"}}
        orgPropsGetAll {:type "getAllOrgs"
                        :user "uberuser"}
        orgCreated (run orgPropsCreate)
        orgReceived (run orgPropsGet)
        orgUpdated (run orgPropsUpdate)
        orgReceivedAll (run orgPropsGetAll)]

    (is (true? (:success orgCreated)))
    (is (true? (:success orgReceived)))
    (is (true? (:success orgUpdated)))
    (is (true? (:success orgReceivedAll)))
    (is (= (:org orgReceived) (:orgprops orgPropsCreate)))
    (is (not (empty? (filter #(= (:orgprops orgPropsUpdate) %) (:items orgReceivedAll))))))

  (let [sitePropsCreate {:type      "createSite"
                         :user      "uberuser"
                         :orgprops  {:orgid "testorg"}
                         :siteprops {:siteid      "testsite"
                                     :name        "Oakmead"
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
                                     :time_zone "America/Los_Angeles"
                                     }}
        sitePropsGet {:type      "getSite"
                      :user      "uberuser"
                      :orgprops  {:orgid "testorg"}
                      :siteprops {:siteid "testsite"}}
        sitePropsUpdate {:type      "updateSite"
                         :user      "uberuser"
                         :orgprops  {:orgid "testorg"}
                         :siteprops {:siteid      "testsite"
                                     :name        "East Arques"
                                     :street1     "1237 East Arques Ave"
                                     :street2     ""
                                     :city        "Sunnyvale"
                                     :state       "CA"
                                     :postal_code "94085"
                                     :country     "USA"
                                     :latitude    "37.381088"
                                     :longitude   "-121.9949372"
                                     :altitude    "125"
                                     :country_code "US"
                                     :time_zone "America/Los_Angeles"}}
        sitePropsGetAll {:type     "getAllSitesForOrg"
                         :user     "uberuser"
                         :orgprops {:orgid "testorg"}}
        siteCreated (run sitePropsCreate)
        siteReceived (run sitePropsGet)
        siteUpdated (run sitePropsUpdate)
        siteReceivedAll (run sitePropsGetAll)]

    (is (true? (:success siteCreated)))
    (is (true? (:success siteReceived)))
    (is (true? (:success siteUpdated)))
    (is (true? (:success siteReceivedAll)))
    (is (= (:site siteReceived) (:siteprops sitePropsCreate)))
                                        ;(is (= (:items siteReceivedAll) (:siteprops sitePropsUpdate))))
    (is (not (empty? (filter #(= (:siteprops sitePropsUpdate) %) (:items siteReceivedAll))))))

  (let [configDefault (nodeconfig/get-full-conf-by-model "unode-v4")
        configPropsCreate {:type "createConfig"
                           :user "uberuser"
                           :orgprops  {:orgid "testorg"}
                           :siteprops {:siteid "testsite"}
                           :configprops (merge configDefault
                                               {:configid "testconfig"
                                                :name "testconfig 1"
                                                :model "unode-v4"
                                                :sensor_rf_pint 1
                                                :sensor_rf_dint 1
                                                :sensor_rf_mode 1})}
        configPropsGet {:type "getConfig"
                        :user "uberuser"
                        :orgprops  {:orgid "testorg"}
                        :siteprops {:siteid "testsite"}
                        :configprops {:configid "testconfig"}}
        configPropsGetAll {:type "getAllConfigs"
                           :user "uberuser"
                           :orgprops  {:orgid "testorg"}
                           :siteprops {:siteid "testsite"}}
        configPropsGetDefault {:type "getDefaultConfigs"
                               :user "uberuser"
                               :nodeprops  {:model "unode-v4"}}
        configPropsUpdate {:type "updateConfig"
                           :user "uberuser"
                           :orgprops  {:orgid "testorg"}
                           :siteprops {:siteid "testsite"}
                           :configprops (merge configDefault
                                               {:configid "testconfig"
                                                :name "testconfig 2"
                                                :sensor_rf_pint 2
                                                :sensor_rf_dint 2
                                                :sensor_rf_mode 2})}
        configCreated (run configPropsCreate)
        configReceived (run configPropsGet)
        configUpdated (run configPropsUpdate)
        configReceivedAll (run configPropsGetAll)
        configReceivedDefault (run configPropsGetDefault)
        configReceivedUpdated (run configPropsGet)]

    (is (true? (:success configCreated)))
    (is (true? (:success configReceived)))
    (is (true? (:success configUpdated)))
    (is (true? (:success configReceivedAll)))
    (is (true? (:success configReceivedUpdated)))
    (is (true? (:success configReceivedDefault)))

    (is (= (:config configReceived) (merge (:configprops configPropsCreate) {:nodes [] :model "unode-v4"})))
    (is (= (:config configReceivedUpdated) (merge (:configprops configPropsUpdate) {:nodes [] :model "unode-v4"})))
    )

  (let [nodePropsCreate {:type      "createNode"
                         :user      "uberuser"
                         :orgprops  {:orgid "testorg"}
                         :siteprops {:siteid "testsite"}
                         :nodeprops {:nodeid      "testnode"
                                     :name        "Test Node"
                                     :model       "unode-v4"
                                     :ip          "192.168.1.1"
                                     :building    "1"
                                     :level       "2"
                                     :latitude    "37.383712"
                                     :longitude   "-121.989921"
                                     :time_zone    "America/Los_Angeles"
                                     :meshId      "Mxeralux1"
                                     :note        "Node Note"
                                     :baseStation "Mac address"
                                     :publicKey   "public key"
                                     :signature   "signature"
                                     :remoteNetwork "XeraL"
                                     :bssid       "bssid"
                                     :configToken "token"
                                     :softwareVersion "e16b568"
                                     :mfgDate     "2014-03-06T22:15:12.074Z"
                                     :circuit     "1"}}
        nodePropsActivate {:type      "activateNode"
                           :user      "uberuser"
                           :orgprops  {:orgid "testorg"}
                           :siteprops {:siteid "testsite"}
                           :nodeprops {:nodeid "testnode"}}
        nodePropsGet {:type      "getNode"
                      :user      "uberuser"
                      :orgprops  {:orgid "testorg"}
                      :siteprops {:siteid "testsite"}
                      :nodeprops {:nodeid "testnode"}}
        nodePropsUpdate {:type      "updateNode"
                         :user      "uberuser"
                         :orgprops  {:orgid "testorg"}
                         :siteprops {:siteid "testsite"}
                         :nodeprops {:nodeid      "testnode"
                                     :name        "Test Node1"
                                     :model       "unode-v4"
                                     :ip          "192.168.1.2"
                                     :building    "2"
                                     :level       "3"
                                     :latitude    "37.3837121"
                                     :longitude   "-121.9899210"
                                     :time_zone    "America/Los_Angeles"
                                     :meshId      "Mxeralux1"
                                     :note        "Node Note"
                                     :baseStation "Mac address"
                                     :publicKey   "public key"
                                     :signature   "signature"
                                     :remoteNetwork "XeraL"
                                     :bssid       "bssid"
                                     :configToken "token"
                                     :softwareVersion "e16b568"
                                     :mfgDate     "2014-03-06T22:15:12.074Z"
                                     :circuit     "1"
                                     :country_code "US"}}
        nodePropsGetAll {:type     "getAllNodesForSite"
                         :user     "uberuser"
                         :orgprops {:orgid "testorg"}
                         :siteprops {:siteid "testsite"}}
        createBulkNode {:type "createBulkNode"
                        :user "uberuser"
                        :nodeprops {:nodes [{:nodeid "bulk1" :siteid "testsite" :orgid "testorg" :model "unode-v4"}
                                            {:nodeid "bulk2" :siteid "testsite" :orgid "testorg" :model "unode-v4"}]}}
        nodeCreated (run nodePropsCreate)
        nodeActivated (run nodePropsActivate)
        nodeReceived (run nodePropsGet)
        nodeUpdated (run nodePropsUpdate)
        nodeReceivedAll (run nodePropsGetAll)
        nodeBulkCreated (run createBulkNode)]
    (is (true? (:success nodeCreated)))
    (is (true? (:success nodeActivated)))
    (is (true? (:success nodeReceived)))
    (is (true? (:success nodeUpdated)))
    (is (true? (:success nodeReceivedAll)))

    (is (true? (:success nodeBulkCreated)))
    (is (true? (= [{:orgid "testorg" :siteid "testsite"}] (al/get_node_hierarchy "bulk1"))))
    (is (true? (= [{:orgid "testorg" :siteid "testsite"}] (al/get_node_hierarchy "bulk2"))))

    (is (= (-> (:node nodeReceived)
               (dissoc :groupidlist
                       :scheduleid))
           (-> (:nodeprops nodePropsCreate)
               (assoc :groupnamelist ["Site Lighting Group"]
                      :schedulename "Default schedule"
                      :country_code "US"
                      :configStatus "pending")))))

  (let [firmwarePropsCreate {:type "createFirmware"
                             :user "uberuser"
                             :firmwareprops {:firmwareid "A-hash1"
                                             :name "My firmware"
                                             :release "1"
                                             :released false
                                             :commit "hash1"
                                             :deprecated false
                                             :version "1.hash1"
                                             :checksum "sum1"
                                             :builder "My Builder"
                                             :build_date "today"
                                             :image_size 1024}}
        firmwarePropsGet {:type "getFirmware"
                          :user "uberuser"
                          :firmwareprops {:firmwareid "A-hash1"}}
        firmwarePropsGetAll {:type "getAllFirmwares"
                             :user "uberuser"}
        firmwarePropsUpdate {:type "updateFirmware"
                             :user "uberuser"
                             :firmwareprops {:firmwareid "A-hash1"
                                             :name "My firmware"
                                             :release "1"
                                             :released true
                                             :commit "hash"
                                             :deprecated false
                                             :version "1.hash1"
                                             :checksum "sum1"
                                             :builder "My Builder"
                                             :build_date "today"
                                             :image_size 2048}}
        firmwarePropsDelete {:type "deleteFirmware"
                             :user "uberuser"
                             :firmwareprops {:firmwareid "A-hash1"}}
        firmwarePropsAssignToSite {:type "assignFirmwareToSite"
                                   :user "uberuser"
                                   :firmwareprops {:firmwareid "A-hash1"}
                                   :siteprops {:siteid "testsite"}}
        firmwarePropsAssignToNode {:type "assignFirmwareToNode"
                                   :user "uberuser"
                                   :firmwareprops {:firmwareid "A-hash1"}
                                   :siteprops {:siteid "testsite"}
                                   :nodeprops {:nodeid "ubernode"}}

        firmwareCreated (run firmwarePropsCreate)
        firmwareReceived (run firmwarePropsGet)
        firmwareUpdated (run firmwarePropsUpdate)
        firmwareReceivedAll (run firmwarePropsGetAll)
        firmwareAssignToSite (run firmwarePropsAssignToSite)
        firmwareAssignToNode (run firmwarePropsAssignToNode)
        firmwareDeleted (run firmwarePropsDelete)
        ]

    (is (true? (:success firmwareCreated)))
    (is (true? (:success firmwareReceived)))
    (is (true? (:success firmwareUpdated)))
    (is (true? (:success firmwareReceivedAll)))
    (is (true? (:success firmwareAssignToSite)))
    (is (true? (:success firmwareAssignToNode)))
    (is (true? (:success firmwareDeleted)))
    (is (= (:firmware firmwareReceived) (merge {:version "1.hash1"} (:firmwareprops firmwarePropsCreate)))))

  (let [nodeCreate {:type      "createNode"
                    :user      "uberuser"
                    :orgprops  {:orgid "testorg"}
                    :siteprops {:siteid "testsite"}
                    :nodeprops {:nodeid      "groupnode"
                                :name        "Group Node"
                                :model       "unode-v4"
                                :latitude    "37.383712"
                                :longitude   "-121.989921"}}
        groupPropsCreate {:type "createGroup"
                          :user "uberuser"
                          :orgprops {:orgid "testorg"}
                          :siteprops {:siteid "testsite"}
                          :groupprops {:groupid "group1"
                                       :description "Group 1"
                                       :nodeList ["testnode"]
                                       :name "My group"
                                       :type "organizational"}}
        lightingGroupPropsCreate {:type "createGroup"
                                  :user "uberuser"
                                  :orgprops {:orgid "testorg"}
                                  :siteprops {:siteid "testsite"}
                                  :groupprops {:groupid "lightinggroup1"
                                               :description "Group 1"
                                               :nodeList ["testnode"]
                                               :name "My group"
                                               :type "lighting"}}
        groupPropsGet {:type "getGroup"
                       :user "uberuser"
                       :groupprops {:groupid "group1"}}
        groupPropsGetAll {:type "getAllGroups"
                          :user "uberuser"
                          :siteprops {:siteid "testsite"}}
        groupPropsUpdate {:type "updateGroup"
                          :user "uberuser"
                          :orgprops {:orgid "uberorg"}
                          :siteprops {:siteid "testsite"}
                          :groupprops {:groupid "group1"
                                       :description "Group 1"
                                       :nodeList ["groupnode" "testnode"]
                                       :name "My group Updated"
                                       :type "organizational"}}
        groupRemoveNode {:type "removeNodeFromGroup"
                         :user "uberuser"
                         :orgprops {:orgid "uberorg"}
                         :siteprops {:siteid "testsite"}
                         :groupprops {:groupid "group1"}
                         :nodeprops {:nodeid "groupnode"}}
        groupAddNode {:type "addNodeToGroup"
                      :user "uberuser"
                      :orgprops {:orgid "testorg"}
                      :siteprops {:siteid "testsite"}
                      :groupprops {:groupid "group1"}
                      :nodeprops {:nodeid "groupnode"}}
        vidNodeCreate {:type      "createNode"
                          :user      "uberuser"
                          :orgprops  {:orgid "testorg"}
                          :siteprops {:siteid "testsite"}
                          :nodeprops {:nodeid      "groupvidnode"
                                      :name        "Group Node"
                                      :model       "falcon-q"
                                      :latitude    "37.383712"
                                      :longitude   "-121.989921"}}
        groupAddVidNode {:type "addNodeToGroup"
                      :user "uberuser"
                      :orgprops {:orgid "testorg"}
                      :siteprops {:siteid "testsite"}
                      :groupprops {:groupid "lightinggroup1"}
                      :nodeprops {:nodeid "groupvidnode"}}

        nodeCreated (run nodeCreate)
        groupCreated (run groupPropsCreate)
        lightingGroupCreated (run lightingGroupPropsCreate)
        groupReceived (run groupPropsGet)
        groupUpdated (run groupPropsUpdate)
        groupReceivedUpdated (run groupPropsGet)
        groupRemove (run groupRemoveNode)
        groupReceivedRemoved (run groupPropsGet)
        groupAdd (run groupAddNode)
        groupReceivedAll (run groupPropsGetAll)
        vidNodeCreated (run vidNodeCreate)
        groupVidAdd (run groupAddVidNode)]

    (is (true? (:success nodeCreated)))
    (is (true? (:success groupCreated)))
    (is (true? (:success lightingGroupCreated)))
    (is (true? (:success groupReceived)))
    (is (true? (:success groupUpdated)))
    (is (true? (:success groupRemove)))
    (is (true? (:success groupAdd)))
    (is (true? (:success groupReceivedAll)))
    (is (false? (:success groupVidAdd)))
    (is (= (:group groupReceived)
           (assoc (:groupprops groupPropsCreate)
                  :etdhprofiles []
                  :dhprofiles []
                  :pdprofiles []
                  :schedules []
                  :nodes [{:model "unode-v4", :nodeid "testnode"}])))

    (is (= (:group groupReceivedUpdated)
           (assoc (:groupprops groupPropsUpdate)
                  :etdhprofiles []
                  :dhprofiles []
                  :pdprofiles []
                  :schedules []
                  :nodes [{:model "unode-v4", :nodeid "groupnode"} {:model "unode-v4", :nodeid "testnode"}])))
    (is (= (:group groupReceivedRemoved)
           (assoc (:groupprops groupPropsUpdate)
             :nodeList ["testnode"]
             :etdhprofiles []
             :dhprofiles []
             :pdprofiles []
             :schedules []
             :nodes [{:model "unode-v4", :nodeid "testnode"}])))
    (is (= (:items groupReceivedAll [(:groupprops groupPropsUpdate)]))))

  (let [fixturePropsCreate {:type "createFixture"
                            :user "uberuser"
                            :siteprops {:siteid "testsite"}
                            :fixtureprops {:fixtureid "fixture1"
                                           :name "My fixture"
                                           :fixtureType "Lamp"
                                           :MaxPower0 "0"
                                           :MaxPower10 "10"
                                           :MaxPower50 "50"
                                           :MaxPower100 "100"
                                           :MinPower100 "100"
                                           :MinPower50 "50"
                                           :MinPower10 "10"
                                           :MinPower0 "0"
                                           :PowerDraw "draw"
                                           :MinimumLightLevelForFailureDetection "10"
                                           :nemasocket "nemasocket"
                                           :description "description"
                                           :manufacturer "manufacturer"
                                           :manufacturersku "manufacturersku"
                                           :BallastCost "10"
                                           :BulbCost "5"
                                           :LegacyPowerDraw "50"
                                           :DailyOperatingTime "10"}}
        fixturePropsGet {:type "getFixture"
                         :user "uberuser"
                         :fixtureprops {:fixtureid "fixture1"}}
        fixturePropsGetAll {:type "getAllFixtures"
                            :user "uberuser"
                            :siteprops {:siteid "testsite"}}
        fixturePropsUpdate {:type "updateFixture"
                            :user "uberuser"
                            :siteprops {:siteid "testsite"}
                            :fixtureprops {:fixtureid "fixture1"
                                           :name "My fixture Updated"
                                           :fixtureType "New Lamp"
                                           :MaxPower0 "10"
                                           :MaxPower10 "100"
                                           :MaxPower50 "500"
                                           :MaxPower100 "1000"
                                           :MinPower100 "1000"
                                           :MinPower50 "500"
                                           :MinPower10 "100"
                                           :MinPower0 "10"
                                           :PowerDraw "draw x2"
                                           :MinimumLightLevelForFailureDetection "100"
                                           :nemasocket "nemasocket"
                                           :description "description"
                                           :manufacturer "manufacturer"
                                           :manufacturersku "manufacturersku"
                                           :BallastCost "15"
                                           :BulbCost "10"
                                           :LegacyPowerDraw "100"
                                           :DailyOperatingTime "0"}}
        fixtureAssignmentNode {:type "assignFixtureToNode"
                               :user "uberuser"
                               :siteprops {:siteid "testsite"}
                               :nodeprops {:nodeid "testnode"}
                               :fixtureprops {:fixtureid "fixture1"}}
        fixtureAssignmentNodes {:type "assignFixtureToNodes"
                                :user "uberuser"
                                :siteprops {:siteid "testsite"}
                                :nodeprops {:nodeids ["testnode"]}
                                :fixtureprops {:fixtureid "fixture1"}}
        fixtureAssignmentSite {:type "assignFixtureToSite"
                               :user "uberuser"
                               :siteprops {:siteid "testsite"}
                               :fixtureprops {:fixtureid "fixture1"}}
        fixtureAssignmentGroup {:type "assignFixtureToGroup"
                                :user "uberuser"
                                :siteprops {:siteid "testsite"}
                                :groupprops {:groupids ["group1"]}
                                :fixtureprops {:fixtureid "fixture1"}}
        fixturePropsDelete {:type "deleteFixture"
                            :user "uberuser"
                            :siteprops {:siteid "testsite"}
                            :fixtureprops {:fixtureid "fixture1"}}

        fixtureCreated (run fixturePropsCreate)
        fixtureReceived (run fixturePropsGet)
        fixtureUpdated (run fixturePropsUpdate)
        fixtureReceivedAll (run fixturePropsGetAll)
        fixtureAssignNode (run fixtureAssignmentNode)
        fixtureAssignNodes (run fixtureAssignmentNodes)
        fixtureAssignSite (run fixtureAssignmentSite)
        fixtureAssignGroup (run fixtureAssignmentGroup)
        fixtureDeleted (run fixturePropsDelete)]

    (is (true? (:success fixtureCreated)))
    (is (true? (:success fixtureReceived)))
    (is (true? (:success fixtureUpdated)))
    (is (true? (:success fixtureReceivedAll)))
    (is (true? (:success fixtureAssignNode)))
    (is (true? (:success fixtureAssignNodes)))
    (is (true? (:success fixtureAssignSite)))
    (is (true? (:success fixtureAssignGroup)))
    (is (= (:result fixtureAssignNode) {:fixtureid "fixture1" :nodeid "testnode"}))
    (is (= (:result fixtureAssignNodes) {:fixtureid "fixture1" :nodeids ["testnode"]}))
    (is (= (:result fixtureAssignSite) {:fixtureid "fixture1" :siteid "testsite"}))
    (is (= (:result fixtureAssignGroup) {:fixtureid "fixture1" :groupids ["group1"]}))
    (is (true? (:success fixtureDeleted)))
    (is (= (dissoc (:fixture fixtureReceived) :site :groups) (:fixtureprops fixturePropsCreate)))
    (is (= (:items fixtureReceivedAll) [(:fixtureprops fixturePropsUpdate)])))

  (let [overlayPropsCreate {:type "createOverlay"
                            :user "uberuser"
                            :siteprops {:siteid "testsite"}
                            :overlayprops {:overlayid "overlay1"
                                           :fileName "filename"
                                           :buildingLevel "1"
                                           :description "My overlay"
                                           :users "admin"
                                           :imageBounds "30x30"
                                           :imageType "png"
                                           :imageData "data"}}
        overlayPropsGet {:type "getOverlay"
                         :user "uberuser"
                         :overlayprops {:overlayid "overlay1"}}
        overlayPropsGetAll {:type "getAllOverlays"
                            :user "uberuser"
                            :siteprops {:siteid "testsite"}}
        overlayPropsUpdate {:type "updateOverlay"
                            :user "uberuser"
                            :siteprops {:siteid "testsite"}
                            :overlayprops {:overlayid "overlay1"
                                           :fileName "new"
                                           :buildingLevel "2"
                                           :description "My overlay updated"
                                           :users "admin,enduser"
                                           :imageBounds "60x60"
                                           :imageType "jpg"
                                           :imageData "data updated"}}
        overlayPropsDelete {:type "deleteOverlay"
                            :user "uberuser"
                            :siteprops {:siteid "testsite"}
                            :overlayprops {:overlayid "overlay1"}}

        overlayCreated (run overlayPropsCreate)
        overlayReceived (run overlayPropsGet)
        overlayUpdated (run overlayPropsUpdate)
        overlayReceivedAll (run overlayPropsGetAll)
        overlayDeleted (run overlayPropsDelete)]

    (is (true? (:success overlayCreated)))
    (is (true? (:success overlayReceived)))
    (is (true? (:success overlayUpdated)))
    (is (true? (:success overlayReceivedAll)))
    (is (true? (:success overlayDeleted)))
    (is (:overlay overlayReceived) (:overlayprops overlayPropsCreate))
    (is (:items overlayReceivedAll) (:overlayprops overlayPropsUpdate)))

  (let [notificationPropsCreate {:type "createNotification"
                                 :user "uberuser"
                                 :orgprops {:orgid "testorg"}
                                 :siteprops {:siteid "testsite"}
                                 :notificationprops {:notificationid "notification1"
                                                     :scope "testsite"
                                                     :rules "My Rules"
                                                     :hold_off 6
                                                     :resend_interval 5
                                                     :window ""
                                                     :emailUsersList ["uberuser"]
                                                     :smsUsersList ["uberuser"]
                                                     :additionalEmails []
                                                     :active "true"
                                                     :msg "Msg"
                                                     :name "My notification"}}
        notificationPropsGet {:type "getNotification"
                              :user "uberuser"
                              :orgprops {:orgid "testorg"}
                              :siteprops {:siteid "testsite"}
                              :notificationprops {:notificationid "notification1"}}
        notificationPropsGetAll {:type "getAllNotificationsForSite"
                                 :user "uberuser"
                                 :orgprops {:orgid "testorg"}
                                 :siteprops {:siteid "testsite"}}
        notificationPropsUpdate {:type "updateNotification"
                                 :user "uberuser"
                                 :orgprops {:orgid "testorg"}
                                 :siteprops {:siteid "testsite"}
                                 :notificationprops {:notificationid "notification1"
                                                     :scope "testsite"
                                                     :rules "My Rules updated"
                                                     :hold_off 6
                                                     :resend_interval 5
                                                     :window ""
                                                     :emailUsersList ["uberuser"]
                                                     :smsUsersList ["uberuser"]
                                                     :additionalEmails []
                                                     :active "false"
                                                     :msg "Urgent"
                                                     :name "My notification Updated"}}
        notificationPropsDelete {:type "deleteNotification"
                                 :user "uberuser"
                                 :orgprops {:orgid "testorg"}
                                 :siteprops {:siteid "testsite"}
                                 :notificationprops {:notificationid "notification1"}}

        notificationCreated (run notificationPropsCreate)
        notificationReceived (run notificationPropsGet)
        notificationUpdated (run notificationPropsUpdate)
        notificationReceivedAll (run notificationPropsGetAll)
        notificationDeleted (run notificationPropsDelete)
        notification (dissoc (:notification notificationReceived) :created :updated)
        notificationAll (dissoc (first (:items notificationReceivedAll)) :created :updated)
        notificationGetFail (run (assoc-in notificationPropsGet [:notificationprops :notificationid] "fake"))
        notificationUpdateFail (run (assoc-in notificationPropsUpdate [:notificationprops :notificationid] "fake"))
        notificationDeleteFail (run (assoc-in notificationPropsDelete [:notificationprops :notificationid] "fake"))]

    (is (true? (:success notificationCreated)))
    (is (true? (:success notificationReceived)))
    (is (true? (:success notificationUpdated)))
    (is (true? (:success notificationReceivedAll)))
    (is (true? (:success notificationDeleted)))
    (is (= notification (:notificationprops notificationPropsCreate)))
    (is (= notificationAll (:notificationprops notificationPropsUpdate)))


    ; negative testing

    ; get Notification that doesn't exist
    ;(errorf "notificationGetFail %s" notificationGetFail)
    (is (false? (:success notificationGetFail)))

    ; update Notification that doesn't exist
    ;(errorf "notificationUpdateFail %s" notificationUpdateFail)
    (is (false? (:success notificationUpdateFail)))

    ; delete Notification that doesn't exist
    ;(errorf "notificationDeleteFail %s" notificationDeleteFail)
    (is (false? (:success notificationDeleteFail)))
    )

  (let [alertPropsCreate {:type "createAlert"
                          :user "uberuser"
                          :orgprops {:orgid "uberorg"}
                          :siteprops {:siteid "ubersite"}
                          :alertprops { :alertid "alert1"
                                       :name "DeviceAlarm"
                                       :nodeid "ubernode"
                                       :type "HWFail_EEPROM"
                                       :severity "Major"
                                       :msg "Major alert"}}
        alertPropsGet {:type "getAlert"
                       :user "uberuser"
                       :orgprops {:orgid "uberorg"}
                       :siteprops {:siteid "ubersite"}
                       :alertprops {:alertid "alert1"}}
        alertPropsGetAll {:type "getAllAlerts"
                          :user "uberuser"
                          :orgprops {:orgid "uberorg"}
                          :siteprops {:siteid "ubersite"}}
        alertPropsUpdate {:type "updateAlert"
                          :user "uberuser"
                          :orgprops {:orgid "uberorg"}
                          :siteprops {:siteid "ubersite"}
                          :alertprops { :alertid "alert1"
                                       :name "DeviceAlarm"
                                       :nodeid "ubernode"
                                       :type "HWFail_EEPROM"
                                       :severity "Minor"
                                       :msg "Minor alert"}}
        alertPropsDelete {:type "deleteAlert"
                          :user "uberuser"
                          :orgprops {:orgid "uberorg"}
                          :siteprops {:siteid "ubersite"}
                          :alertprops {:alertid "alert1"}}

        alertCreated (run alertPropsCreate)
        alertReceived (run alertPropsGet)
        alertUpdated (run alertPropsUpdate)
        alertReceivedAll (run alertPropsGetAll)
        alertDeleted (run alertPropsDelete)

        alertGetFail (run (assoc-in alertPropsGet [:alertprops :alertid] "fake"))
        alertUpdateFail (run (assoc-in alertPropsUpdate [:alertprops :alertid] "fake"))
        alertDeleteFail (run (assoc-in alertPropsDelete [:alertprops :alertid] "fake"))
        ]

    (is (true? (:success alertCreated)))
    (is (true? (:success alertReceived)))
    (is (true? (:success alertUpdated)))
    (is (true? (:success alertReceivedAll)))
    (is (true? (:success alertDeleted)))
    (is (:alert alertReceived) (:alertprops alertPropsCreate))
    (is (:items alertReceivedAll) (:alertprops alertPropsUpdate))

    ; negative testing

    ; get Notification that doesn't exist
    ;(errorf "alertGetFail %s" alertGetFail)
    (is (false? (:success alertGetFail)))

    ; update Notification that doesn't exist
    ;(errorf "alertUpdateFail %s" alertUpdateFail)
    (is (false? (:success alertUpdateFail)))

    ; delete Notification that doesn't exist
    ;(errorf "alertDeleteFail %s" alertDeleteFail)
    (is (false? (:success alertDeleteFail)))
    )

  (let [userPropsCreated {:type     "createUser"
                          :user      "uberuser"
                          :orgprops  {:orgid "testorg"}
                          :userprops {:userid   "testuser@sensity.com"
                                      :email    "testuser@sensity.com"
                                      :name     "Test User"
                                      :roles    "sensity_admin"
                                      :sites    "testsite"
                                      :title    "User in Test"
                                      :phone    "8675309"}}
        userPropsGet {:type      "getUser"
                      :user      "uberuser"
                      :orgprops  {:orgid "testorg"}
                      :userprops {:userid "testuser@sensity.com"}}
        userPropsUpdated {:type      "updateUser"
                          :user      "uberuser"
                          :orgprops  {:orgid "testorg"}
                          :userprops {:userid "testuser@sensity.com"
                                      :email    "testuser@sensity.com"
                                      :name     "Test User Updated"
                                      :roles    "sensity_admin"
                                      :sites    "testsite"
                                      :title    "User in Test"
                                      :phone    "8675309"}}
        userPropsDeleted {:type "deleteUser"
                          :user "uberuser"
                          :orgprops {:orgid "testorg"}
                          :userprops {:userid "testuser@sensity.com"}}

        userCreated (run userPropsCreated)
        userReceived (run userPropsGet)
        userUpdated (run userPropsUpdated)
        userDeleted (run userPropsDeleted)
        user-received (dissoc (:user userReceived) :created :updated)
        user-updated (dissoc (:user userUpdated) :created :updated)]

    (is (true? (:success userCreated)))
    (is (true? (:success userReceived)))
    (is (true? (:success userUpdated)))
    (is (true? (:success userDeleted)))
    (is (= user-received (:userprops userPropsCreated)))
    (is (= user-updated (:userprops userPropsUpdated))))

                                        ; Lighting Overrides
  (let [lightingGroup {:type "createGroup"
                       :user "uberuser"
                       :orgprops  {:orgid "testorg"}
                       :siteprops {:siteid "testsite"}
                       :groupprops {:groupid "lighting1"
                                    :nodeList "testnode"
                                    :name "Lighting Group"
                                    :type "organizational"}}
        lightingByNode {:type     "lighting-control"
                        :user      "uberuser"
                        :orgprops  {:orgid "testorg"}
                        :siteprops {:siteid "testsite"}
                        :nodeprops {:type "LightingForceState"
                                    :nodeid "testnode"
                                    :level 80
                                    :timeout 30}}
        lightingBySite {:type     "lighting-control-site"
                        :user      "uberuser"
                        :orgprops  {:orgid "testorg"}
                        :siteprops {:siteid "testsite"}
                        :nodeprops {:type "LightingForceState"
                                    :nodeid "testnode"
                                    :level 80
                                    :timeout 30}}
        lightingByGroup {:type     "lighting-control-group"
                         :user      "uberuser"
                         :orgprops  {:orgid "testorg"}
                         :siteprops {:siteid "testsite"}
                         :groupprops {:groupid "lighting1"}
                         :nodeprops {:type "LightingForceState"
                                     :nodeid "testnode"
                                     :level 80
                                     :timeout 30}}
        clearByNode {:type     "lighting-control"
                     :user      "uberuser"
                     :orgprops  {:orgid "testorg"}
                     :siteprops {:siteid "testsite"}
                     :nodeprops {:type "LightingSetAuto"
                                 :nodeid "testnode"
                                 :level 80
                                 :timeout 30}}
        clearBySite {:type     "lighting-control-site"
                     :user      "uberuser"
                     :orgprops  {:orgid "testorg"}
                     :siteprops {:siteid "testsite"}
                     :nodeprops {:type "LightingSetAuto"
                                 :nodeid "testnode"
                                 :level 80
                                 :timeout 30}}
        clearByGroup {:type     "lighting-control-group"
                      :user      "uberuser"
                      :orgprops  {:orgid "testorg"}
                      :siteprops {:siteid "testsite"}
                      :groupprops {:groupid "lighting1"}
                      :nodeprops {:type "LightingSetAuto"
                                  :nodeid "testnode"
                                  :level 80
                                  :timeout 30}}
        groupDone (run lightingGroup)
        lightNodeDone (run lightingByNode)
        lightSiteDone (run lightingBySite)
        lightGroupDone (run lightingByGroup)
        clearedNodeDone (run clearByNode)
        clearedSiteDone (run clearBySite)
        clearedGroupDone (run clearByGroup)]


    (is (true? (:success groupDone)))
    (is (true? (:success lightNodeDone)))
    (is (true? (:success lightSiteDone)))
    (is (true? (:success lightGroupDone)))
    (is (true? (:success clearedNodeDone)))
    (is (true? (:success clearedSiteDone)))
    (is (true? (:success clearedGroupDone)))
    )

  (let [schedulePropsCreated {:type     "createSchedule"
                              :user      "uberuser"
                              :orgprops  {:orgid "testorg"}
                              :siteprops {:siteid "testsite"}
                              :scheduleprops {:scheduleid   "testschedule"
                                              :name "Test Schedule"
                                              :description "This is a test schedule"
                                              :events [
                                                       {:days ["sat", "sun"]
                                                        :photocell_enabled false
                                                        :photocell_highLevel 100
                                                        :photocell_lowLevel 0
                                                        :actions [{:time "sunset"     :level 100}
                                                                  {:time "sunrise+30" :level 0}
                                                                  {:time "23:00:00"   :level 0}
                                                                  {:time "05:00:00"   :level 100}]}
                                                       {:days ["mon" "tue" "wed" "thu" "fri"]
                                                        :photocell_enabled false
                                                        :photocell_highLevel 100
                                                        :photocell_lowLevel 0
                                                        :actions [{:time "sunset"     :level 100}
                                                                  {:time "sunrise+30" :level 0}
                                                                  {:time "23:00:00"   :level 50}
                                                                  {:time "05:00:00"   :level 100}
                                                                  {:time "sunrise"    :level 50}
                                                                  {:time "sunset-30"  :level 50}]}
                                                       ]
                                              :network {:highTime "17:00:00"
                                                        :highLevel 100
                                                        :lowTime "9:00:00"
                                                        :lowLevel 0
                                                        :photocell_enabled true
                                                        :photocell_highLevel 100
                                                        :photocell_lowLevel 0}}}
        scheduleApplyNodes {:type "applyScheduleToNodes"
                            :user "uberuser"
                            :orgprops  {:orgid "testorg"}
                            :siteprops {:siteid "testsite"}
                            :nodeprops {:type "LightingScheduledEvent"
                                        :nodeids ["testnode"]}
                            :scheduleprops {:scheduleid "testschedule"}}
        scheduleApplySite {:type "applyScheduleToSite"
                           :user "uberuser"
                           :orgprops  {:orgid "testorg"}
                           :siteprops {:siteid "testsite"}
                           :nodeprops {:type "LightingScheduledEvent"
                                       :nodeid "testnode"}
                           :scheduleprops {:scheduleid "testschedule"}}
        slg-scheduleid (->> {:type "getAllSchedules"
                             :user "uberuser"
                             :orgprops {:orgid "testorg"}
                             :siteprops {:siteid "testsite"}}
                            run
                            :items
                            (filter (comp #{"Default schedule"}
                                          :name))
                            (map :scheduleid)
                            first)
        scheduleApplySite-2 {:type "applyScheduleToSite"
                             :user "uberuser"
                             :orgprops  {:orgid "testorg"}
                             :siteprops {:siteid "testsite"}
                             :nodeprops {:type "LightingScheduledEvent"
                                         :nodeid "testnode"}
                             :scheduleprops {:scheduleid slg-scheduleid}}
        scheduleApplyGroup {:type "applyScheduleToGroup"
                            :user "uberuser"
                            :orgprops  {:orgid "testorg"}
                            :siteprops {:siteid "testsite"}
                            :groupprops {:groupids "lightinggroup1"}
                            :nodeprops {:type "LightingScheduledEvent"
                                        :nodeid "testnode"}
                            :scheduleprops {:scheduleid "testschedule"}}
        schedulePropsGet {:type      "getSchedule"
                          :user      "uberuser"
                          :orgprops  {:orgid "testorg"}
                          :siteprops {:siteid "testsite"}
                          :scheduleprops {:scheduleid "testschedule"}}
        scheduleGetAll {:type      "getAllSchedules"
                        :user      "uberuser"
                        :orgprops  {:orgid "testorg"}
                        :siteprops {:siteid "testsite"}}



        scheduleGetAll-1 (run scheduleGetAll)
        scheduleCreated (run schedulePropsCreated)
        scheduleGetAll-2 (run scheduleGetAll)
        scheduleReceived (run schedulePropsGet)
                                        ;scheduleUpdated (run schedulePropsUpdated)
        scheduleAppliedNodes (run scheduleApplyNodes)
        scheduleAppliedSite (run scheduleApplySite)
        scheduleAppliedSite-2 (run scheduleApplySite-2)
        scheduleAppliedGroup (run scheduleApplyGroup)

        delete-this-schedule (create-test-schedule "testorg"
                                                   "testsite")
        schedulePropsDeleted {:type "deleteSchedule"
                              :user "uberuser"
                              :orgprops {:orgid "testorg"}
                              :siteprops {:siteid "testsite"}
                              :scheduleprops {:scheduleid delete-this-schedule}}


        scheduleDeleted (run schedulePropsDeleted)

        received (vectors-to-sets (:schedule scheduleReceived))
        created (vectors-to-sets (:scheduleprops schedulePropsCreated))
        ]

    ;; Should only have site-schedule
    (is (= 1
           (count (:items scheduleGetAll-1))))
    (is (true? (:success scheduleCreated)))
    ;; Should have site and our newly created schedule
    (is (= 2
           (count (:items scheduleGetAll-2))))
    (is (true? (:success scheduleReceived)))
                                        ;(is (:success scheduleUpdated))
    (is (true? (:success scheduleAppliedNodes)))
    (is (true? (:success scheduleAppliedSite)))
    (is (true? (:success scheduleAppliedSite-2)))
    (is (true? (:success scheduleAppliedGroup)))
    (is (true? (:success scheduleDeleted)))
    ;; Use `set` to ignore ordering of events.
                                        ;(is (= (update-in (:schedule scheduleReceived) [:events] set)
                                        ;       (update-in (:scheduleprops schedulePropsCreated) [:events] set)))
    (is (= (dissoc received :sites :groups :nodes) created))
    )


                                        ; test cleanup
  (let [orgPropsDelete {:type     "deleteOrg"
                        :user     "uberuser"
                        :orgprops {:orgid "testorg"}}
        sitePropsDelete {:type "deleteSite"
                         :user "uberuser"
                         :orgprops  {:orgid "testorg"}
                         :siteprops {:siteid "testsite"}}
        nodePropsDelete {:type "deleteNode"
                         :user "uberuser"
                         :orgprops  {:orgid "testorg"}
                         :siteprops {:siteid "testsite"}
                         :nodeprops {:nodeid "testnode"}}
        groupPropsDelete {:type "deleteGroup"
                          :user "uberuser"
                          :siteprops {:siteid "testsite"}
                          :groupprops {:groupid "group1"}}
        configPropsDelete {:type "deleteConfig"
                           :user "uberuser"
                           :orgprops  {:orgid "testorg"}
                           :siteprops {:siteid "testsite"}
                           :configprops {:configid "testconfig"}}
                                        ; Delete items from bottom to the top
        groupDeleted (run groupPropsDelete)
        configDeleted (run configPropsDelete)
        nodeDeleted (run nodePropsDelete)
        siteDeleted (run sitePropsDelete)
        orgDeleted (run orgPropsDelete)]
    (is (true? (:success orgDeleted)))
    (is (true? (:success siteDeleted)))
    (is (true? (:success nodeDeleted)))
    (is (true? (:success groupDeleted)))
    (is (true? (:success configDeleted)))
    (is (true? (= [{:orgid "Unknown" :siteid "Unknown"}] (al/get_node_hierarchy "testnode"))))))

(deftest addNodeToOtherGroup-test
  (let [orgid-a (create-test-org)
        siteid-a (create-test-site orgid-a)
        nodeid-a (create-test-node orgid-a
                                   siteid-a)

        orgid-b (create-test-org)
        siteid-b (create-test-site orgid-b)
        groupid-other (create-test-group orgid-b
                                         siteid-b)]
    (is (= {:success false
            :error "Could not find Node(s) and Group belonging to Site and Org."}
           (run {:type "addNodeToGroup"
                 :user *user*
                 :orgprops {:orgid orgid-b}
                 :siteprops {:siteid siteid-b}
                 :nodeprops {:nodeid nodeid-a}
                 :groupprops {:groupid groupid-other}})))))

(def ^:dynamic *logger-output*
  (atom []))

(defn fake-logger
  [& args]
  (swap! *logger-output* conj args))

(deftest daylight-harvesting-test
  (testing "Daylight harvesting Cypher properties"
    ;; Daylight harvesting

    ;; Confirm that a node cannot belong to two Daylight Harvesting
    ;; Profiles. Should a new profile claim a node already belonging
    ;; to an existing profile, the node and the existing profile will
    ;; delete their `:HAS`/`:BELONGS_TO` relationships before the node
    ;; and new profile create aforementioned relationships.
    (with-redefs [al/log fake-logger]
      (let [orgid (create-test-org)
            siteid (create-test-site orgid)
            site-lighting-group (->> {:type "getAllGroups"
                                      :user *user*
                                      :orgprops {:orgid orgid}
                                      :siteprops {:siteid siteid}}
                                     run
                                     :items
                                     (map :groupid)
                                     first)
            nodeid (create-test-node orgid siteid)
            groupid (create-test-lighting-group orgid siteid {:nodeList [nodeid]})
            dhprofile {:setPoint "100"
                       :gain "100"
                       :resetTime "8"
                       :minDrive "1"
                       :maxDrive "100"
                       :slewRate "0.05"}
            base-dhprofile {:user *user*
                            :orgprops {:orgid orgid}
                            :siteprops {:siteid siteid}
                            :nodeprops {:type "DaylightHarvesting"}}
            node-profile #(-> base-dhprofile
                              (assoc :type "applyDHtoNodes")
                              (assoc-in [:nodeprops :nodeids] [nodeid])
                              (assoc-in [:dhprofileprops :dhprofileid] %))
            group-profile #(-> base-dhprofile
                               (assoc :type "applyDHtoGroup")
                               (assoc-in [:groupprops :groupids] [groupid])
                               (assoc-in [:dhprofileprops :dhprofileid] %))
            site-profile #(-> base-dhprofile
                              (assoc :type "applyDHtoSite")
                              (assoc-in [:dhprofileprops :dhprofileid] %))

            assign-profile-to-node #(-> base-dhprofile
                                        (assoc :type "applyDHtoNodes"
                                               :dhprofileprops %1)
                                        (assoc-in [:dhprofileprops :dhprofileid] %2)
                                        (assoc-in [:nodeprops :nodeids] [%3]))
            get-dhprofile #(assoc base-dhprofile
                                  :type "getDHProfile"
                                  :dhprofileprops {:dhprofileid %})
            update-dhprofile #(assoc base-dhprofile
                                     :type "updateDHProfile"
                                     :dhprofileprops %)
            delete-dhprofile #(assoc base-dhprofile
                                     :type "deleteDHProfile"
                                     :dhprofileprops {:dhprofileid %})
            create-dhprofile #(assoc base-dhprofile
                                     :type "createDHProfile"
                                     :dhprofileprops %)
            get-all-profiles (assoc base-dhprofile
                                    :type "getAllDHProfiles")
            calibrate-profile #(assoc base-dhprofile
                                      :type "calibrateDHProfile"
                                      :dhprofileprops {:dhprofileid %})
            nodes-for-profile (fn [dhprofileid]
                                (->> (str "
MATCH (:DHProfile {dhprofileid: \"" dhprofileid "\"})-[:DH]-(n:Node)
WITH {nodeid: n.nodeid} AS nodes
RETURN DISTINCT nodes
")
                                     neo4j/executeQuery
                                     json/read-str
                                     keywordize-keys
                                     :nodes))]

        (testing "CRUD operations:"

          (testing "Get-all should be initially empty"
            (is (= {:items []
                    :success true}
                   (run get-all-profiles))))

          (testing "Creating"
            (testing "a profile can be immediately fetched"
              (is (= {:dhprofile (-> dhprofile
                                     (assoc :dhprofileid "new-profile"
                                            :autocalibrate true))
                      :success true}
                     (run (create-dhprofile (assoc dhprofile
                                                   :dhprofileid "new-profile")))))
              (is (= {:dhprofile (-> dhprofile
                                     (assoc :dhprofileid "new-profile"
                                            :autocalibrate true
                                            :sites []
                                            :groups []
                                            :nodes []))
                      :success true}
                     (run (get-dhprofile "new-profile"))))))

          (testing "Get-all should contain the new profile"
            (is (= {:items [(-> dhprofile
                                (assoc :dhprofileid "new-profile"
                                       :autocalibrate true
                                       :sites []
                                       :groups []
                                       :nodes []))]
                    :success true}
                   (run get-all-profiles))))

          (testing "Get"
            (testing "errors when profile does not exist"
              (is (= {:dhprofile {}
                      :success false
                      :error "DHProfile 'profile-does-not-exist' not found"
                      :status 404}
                     (run (get-dhprofile "profile-does-not-exist")))))
            (testing "correctly returns existing profiles"
              (is (= {:dhprofile (-> dhprofile
                                     (assoc :dhprofileid "new-profile"
                                            :autocalibrate true
                                            :sites []
                                            :groups []
                                            :nodes []))
                      :success true}
                     (run (get-dhprofile "new-profile"))))))

          (testing "Updating"
            (testing "a profile that does not exist will not create it"
              (is (= {:success false}
                     (run (update-dhprofile (assoc dhprofile
                                                   :dhprofileid "profile-does-not-exist")))))
              (is (= {:dhprofile {}
                      :success false
                      :error "DHProfile 'profile-does-not-exist' not found"
                      :status 404}
                     (run (get-dhprofile "profile-does-not-exist")))))
            (testing "a profile that does exist can be confirmed"
              (is (= {:dhprofile (-> dhprofile
                                     (assoc :dhprofileid "new-profile"
                                            :autocalibrate true
                                            :sites []
                                            :groups []
                                            :nodes []))
                      :site (select-keys default-test-site [:latitude
                                                            :longitude])
                      :success true}
                     (run (update-dhprofile (assoc dhprofile
                                                   :dhprofileid "new-profile")))))
              (is (= {:dhprofile (-> dhprofile
                                     (assoc :dhprofileid "new-profile"
                                            :autocalibrate true
                                            :sites []
                                            :groups []
                                            :nodes []))
                      :success true}
                     (run (get-dhprofile "new-profile"))))))

          (testing "Deleting"
            (testing "a profile that does not exist"
              (is (= {:error "Could not find the specified dhprofile"
                      :success false}
                     (run (delete-dhprofile "DNE")))))
            (testing "an existing profile cannot be fetched again"
              (is (= {:success true}
                     (run (delete-dhprofile "new-profile"))))
              (is (= {:dhprofile {}
                      :success false
                      :error "DHProfile 'new-profile' not found"
                      :status 404}
                     (run (get-dhprofile "new-profile")))))))

        (testing "Profile application"
          (testing "will fail when the profile does not exist"
            (binding [*logger-output* (atom nil)]
              (try
                (run (node-profile "DNE"))
                (catch clojure.lang.ExceptionInfo e
                       (is (= "DHProfile with id=DNE not found"
                              (.getMessage e)))))

              (is (nil? @*logger-output*)))
            (is (nil? (nodes-for-profile "DNE"))))

          (testing "will pass when assigning a new profile"
            (let [test-node-id (.. java.util.UUID
                                   randomUUID
                                   toString)]
              (is (= {:dhprofile (-> dhprofile
                                     (assoc :dhprofileid test-node-id
                                            :autocalibrate true))
                      :success true}
                     (run (create-dhprofile (assoc dhprofile
                                                   :dhprofileid test-node-id)))))

              (testing "to a node"
                (binding [*logger-output* (atom [])]
                  (is (= {:result (-> dhprofile
                                      (assoc :dhprofileid test-node-id
                                             :autocalibrate true
                                             :sites []
                                             :groups []
                                             :nodes [{:nodeid nodeid}]))
                            :success true}
                           (run (node-profile test-node-id))))
                  (is (= [["user" {:userid "uberuser"
                                   :activity "applyDHtoNodes"
                                   :targettype "Nodes"
                                   :targetid nodeid
                                   :message {:dhprofileid test-node-id
                                             :nodeids [nodeid]}}]
                          ["site" {:siteid siteid
                                   :userid "uberuser"
                                   :activity "applyDHtoNodes"
                                   :targettype "Nodes"
                                   :targetid nodeid
                                   :message {:dhprofileid test-node-id
                                             :nodeids [nodeid]}}]]
                         @*logger-output*)))
                (is (= (nodes-for-profile test-node-id)
                       {:nodeid nodeid})))

              (testing "to a group"
                (binding [*logger-output* (atom [])]
                  (is (= {:result (-> dhprofile
                                      (assoc :dhprofileid test-node-id
                                             :autocalibrate true
                                             :sites []
                                             :groups [{:groupid groupid
                                                       :name "Test Lighting Group"}]
                                             :nodes [{:nodeid nodeid}]))
                          :success true}
                           (run (group-profile test-node-id))))
                  (is (= [["user" {:userid "uberuser"
                                   :activity "applyDHtoGroup"
                                   :targettype "Group"
                                   :targetid groupid
                                   :message {:dhprofileid test-node-id
                                             :siteid siteid
                                             :groupids [groupid]}}]
                          ["site" {:siteid siteid
                                   :userid "uberuser"
                                   :activity "applyDHtoGroup"
                                   :targettype "Group"
                                   :targetid groupid
                                   :message {:dhprofileid test-node-id
                                             :siteid siteid
                                             :groupids [groupid]}}]
                          ["user" {:userid "uberuser"
                                   :activity "applyDHtoNodes"
                                   :targettype "Nodes"
                                   :targetid nodeid
                                   :message {:dhprofileid test-node-id
                                             :nodeids [nodeid]}}]
                          ["site" {:siteid siteid
                                   :userid "uberuser"
                                   :activity "applyDHtoNodes"
                                   :targettype "Nodes"
                                   :targetid nodeid
                                   :message {:dhprofileid test-node-id
                                             :nodeids [nodeid]}}]]
                         @*logger-output*)))
                (is (= {:nodeid nodeid}
                       (nodes-for-profile test-node-id))))

              (testing "to a site"
                (binding [*logger-output* (atom [])]
                  (is (= {:result (-> dhprofile
                                      (assoc :dhprofileid test-node-id
                                             :autocalibrate true
                                             :sites [{:groupid site-lighting-group
                                                      :name "Site Lighting Group"}]
                                             :groups [{:groupid groupid
                                                       :name "Test Lighting Group"}]
                                             :nodes [{:nodeid nodeid}]))
                            :success true}
                           (run (site-profile test-node-id))))
                  (is (= [["user" {:userid "uberuser"
                                   :activity "applyDHtoSite"
                                   :targettype "Site"
                                   :targetid siteid
                                   :message {:dhprofileid test-node-id
                                             :siteid siteid}}]
                          ["site" {:siteid siteid
                                   :userid "uberuser"
                                   :activity "applyDHtoSite"
                                   :targettype "Site"
                                   :targetid siteid
                                   :message {:dhprofileid test-node-id
                                             :siteid siteid}}]]
                         @*logger-output*)))
                (is (= {:nodeid nodeid}
                       (nodes-for-profile test-node-id)))))))

        (neo4j/executeQuery "MATCH (p:DHProfile) DETACH DELETE p")))))

(deftest disallow-setting-pdprofile-slg-test
  (testing "Disallow setting pdprofile to site lighting group"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          site-lighting-group (->> {:type "getAllGroups"
                                    :user *user*
                                    :orgprops {:orgid orgid}
                                    :siteprops {:siteid siteid}}
                                   run
                                   :items
                                   (map :groupid)
                                   first)
          pdprofileid (create-test-pdprofile orgid siteid)
          pdprofile {:minLevel "23"
                     :maxLevel "79"
                     :beginTime "00:00:00"
                     :endTime "23:59:59"
                     :radius "15"
                     :detection_duration "12"}
          base-pdprofile (-> {:pdprofileprops pdprofile}
                             (assoc :user *user*)
                             (assoc-in [:orgprops :orgid] orgid)
                             (assoc-in [:siteprops :siteid] siteid)
                             (assoc-in [:nodeprops :type] "ProximityDimming"))
          group-profile #(-> base-pdprofile
                             (assoc :type "applyPDtoGroup")
                             (assoc-in [:groupprops :groupids] [site-lighting-group])
                             (assoc-in [:pdprofileprops :pdprofileid] %))
          ]
      (testing "fails when setting pd profile"
        (is (= {:error "Updating a site lighting group with a proximity dimming profile is not supported." :success false}
               (select-keys (run (group-profile pdprofileid)) [:error :success])))))))

(deftest disallow-setting-pdprofile-sch-nodes
  (testing "Disallow setting pdprofile to site lighting group"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          nodeid (create-test-node orgid siteid {:model "cnext"})
          groupid (create-test-lighting-group orgid siteid {:nodeList [nodeid]})
          pdprofileid (create-test-pdprofile orgid siteid)
          pdprofile {:minLevel "23"
                     :maxLevel "79"
                     :beginTime "00:00:00"
                     :endTime "23:59:59"
                     :radius "15"
                     :detection_duration "12"}
          base-pdprofile (-> {:pdprofileprops pdprofile}
                             (assoc :user *user*)
                             (assoc-in [:orgprops :orgid] orgid)
                             (assoc-in [:siteprops :siteid] siteid)
                             (assoc-in [:nodeprops :type] "ProximityDimming"))
          group-profile #(-> base-pdprofile
                             (assoc :type "applyPDtoGroup")
                             (assoc-in [:groupprops :groupids] [groupid])
                             (assoc-in [:pdprofileprops :pdprofileid] %))
          ]
      (testing "fails when setting pd profile"
        (is (= {:error "Applying a proximity dimming profile to nodes containing a NGCN node is not currently supported." :success false}
               (select-keys (run (group-profile pdprofileid)) [:error :success])))))))

(deftest apply-proximity-dimming-test
  (testing "Proximity lighting"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          site-lighting-group (->> {:type "getAllGroups"
                                    :user *user*
                                    :orgprops {:orgid orgid}
                                    :siteprops {:siteid siteid}}
                                   run
                                   :items
                                   (map :groupid)
                                   first)
          [nodeid1 nodeid2] (repeatedly #(create-test-node orgid siteid))
          groupid (create-test-lighting-group orgid siteid {:nodeList [nodeid1]})
          pdprofile {:minLevel "23"
                     :maxLevel "79"
                     :beginTime "00:00:00"
                     :endTime "23:59:59"
                     :radius "15"
                     :mode "radius"
                     :detection_duration "12"}
          base-pdprofile (-> {:pdprofileprops pdprofile}
                             (assoc :user *user*)
                             (assoc-in [:orgprops :orgid] orgid)
                             (assoc-in [:siteprops :siteid] siteid)
                             (assoc-in [:nodeprops :type] "ProximityDimming"))
          node-profile #(-> base-pdprofile
                            (assoc :type "applyPDtoNodes")
                            (assoc-in [:nodeprops :nodeids] [nodeid1])
                            (assoc-in [:pdprofileprops :pdprofileid] %))
          group-profile #(-> base-pdprofile
                             (assoc :type "applyPDtoGroup")
                             (assoc-in [:groupprops :groupids] [groupid])
                             (assoc-in [:pdprofileprops :pdprofileid] %))
          site-profile #(-> base-pdprofile
                            (assoc :type "applyPDtoSite")
                            (assoc-in [:pdprofileprops :pdprofileid] %))
          get-profile #(assoc base-pdprofile
                              :type "getPDProfile"
                              :pdprofileprops {:pdprofileid %})
          update-profile #(assoc base-pdprofile
                                 :type "updatePDProfile"
                                 :pdprofileprops %)
          delete-profile #(assoc base-pdprofile
                                 :type "deletePDProfile"
                                 :pdprofileprops {:pdprofileid %})
          create-profile #(assoc base-pdprofile
                                 :type "createPDProfile"
                                 :pdprofileprops %)
          get-all-profiles (assoc base-pdprofile
                                  :type "getAllPDProfiles")
          msgpk-output (atom nil)
          nodes-for-profile (fn [pdprofileid]
                              (->> (str "MATCH (:PDProfile {pdprofileid: \"" pdprofileid "\"})<-[r]-(n:Node) RETURN COLLECT ( DISTINCT {nodeid: n.nodeid}) AS nodes")
                                   neo4j/executeQuery
                                   json/read-str
                                   keywordize-keys
                                   :nodes
                                   set))]
      (with-redefs [al/log fake-logger
                    future identity
                    dctrl/query-exec-msgpk #(reset! msgpk-output %&)]

        (testing "CRUD operations:"

          (testing "Get-all should be initially empty"
            (is (= {:items []
                    :success true}
                   (run get-all-profiles))))

          (testing "Creating"
            (testing "a profile can be immediately fetched"
              (is (= {:pdprofile (assoc pdprofile
                                        :pdprofileid "new-profile")
                      :success true}
                     (run (create-profile (assoc pdprofile
                                                 :pdprofileid "new-profile")))))
              (is (= {:pdprofile (assoc pdprofile
                                        :pdprofileid "new-profile"
                                        :sites []
                                        :groups []
                                        :nodes [])
                      :success true}
                     (run (get-profile "new-profile"))))))

          (testing "Get-all should contain the new profile"
            (is (= {:items [(assoc pdprofile
                                   :pdprofileid "new-profile"
                                   :sites []
                                   :groups []
                                   :nodes [])]
                    :success true}
                   (run get-all-profiles))))

          (testing "Get"
            (testing "errors when profile does not exist"
              (is (= {:pdprofile {}
                      :success false
                      :error "PDProfile 'DNE' not found"
                      :status 404}
                     (run (get-profile "DNE")))))
            (testing "correctly returns existing profiles"
              (is (= {:pdprofile (assoc pdprofile
                                        :pdprofileid "new-profile"
                                        :sites []
                                        :groups []
                                        :nodes [])
                      :success true}
                     (run (get-profile "new-profile"))))))

          (testing "Updating"
            (testing "a profile that does not exist will not create it"
              (is (= {:success false}
                     (run (update-profile (assoc pdprofile
                                                 :pdprofileid "DNE")))))
              (is (= {:pdprofile {}
                      :success false
                      :error "PDProfile 'DNE' not found"
                      :status 404}
                     (run (get-profile "DNE")))))
            (testing "a profile that does exist can be confirmed"
              (is (= {:pdprofile (assoc pdprofile
                                        :pdprofileid "new-profile"
                                        :minLevel 29
                                        :sites []
                                        :groups []
                                        :nodes [])
                      :site {:latitude "37.380996"
                             :longitude "-121.992299"}
                      :success true}
                     (run (update-profile (assoc pdprofile
                                                 :pdprofileid "new-profile"
                                                 :minLevel 29)))))
              (is (= {:pdprofile (assoc pdprofile
                                        :pdprofileid "new-profile"
                                        :minLevel 29
                                        :sites []
                                        :groups []
                                        :nodes [])
                      :success true}
                     (run (get-profile "new-profile"))))))

          (testing "Deleting"
            (testing "a profile that does not exist"
              (is (= {:error "Could not find the specified pdprofile"
                      :success false}
                     (run (delete-profile "DNE")))))
            (testing "cannot delete a profile with non-owning org/site"
              (is (= {:error "Could not find the specified pdprofile"
                      :success false}
                     (run (assoc base-pdprofile
                                 :type "deletePDProfile"
                                 :orgprops {:orgid "DNE"}
                                 :siteprops {:siteid "DNE"}
                                 :pdprofileprops {:pdprofileid "new-profile"})))))
            (testing "an existing profile cannot be fetched again"
              (is (= {:success true}
                     (run (delete-profile "new-profile"))))
              (is (= {:pdprofile {}
                      :success false
                      :error "PDProfile 'new-profile' not found"
                      :status 404}
                     (run (get-profile "new-profile")))))))

        (testing "Profile application"
          (testing "will fail when the profile does not exist"
            (binding [*logger-output* (atom [])]
              (try
                (run (node-profile "DNE"))
                (catch clojure.lang.ExceptionInfo e
                       (is (= "PDProfile with id=DNE not found"
                              (.getMessage e)))))
              (is (= []
                     @*logger-output*)))
            (is (nil? @msgpk-output))
            (is (= #{}
                   (nodes-for-profile "DNE"))))

          (testing "will pass when assigning a new profile"
            (let [pdprofileid (.. java.util.UUID
                                  randomUUID
                                  toString)]
              (is (= {:pdprofile (assoc pdprofile
                                        :pdprofileid pdprofileid)
                      :success true}
                     (run (create-profile (assoc pdprofile
                                                 :pdprofileid pdprofileid)))))

              (testing "to a node"
                (binding [*logger-output* (atom [])]
                  (is (= {:result
                          (assoc pdprofile
                                 :pdprofileid pdprofileid
                                 :sites []
                                 :groups []
                                 :nodes [{:nodeid nodeid1}])
                          :success true}
                         (run (node-profile pdprofileid))))
                  (is (= [["user" {:userid "uberuser"
                                   :activity "applyPDtoNodes"
                                   :targettype "Nodes"
                                   :targetid nodeid1
                                   :message {:pdprofileid pdprofileid
                                             :nodeids [nodeid1]}}]
                          ["site" {:siteid siteid
                                   :userid "uberuser"
                                   :activity "applyPDtoNodes"
                                   :targettype "Nodes"
                                   :targetid nodeid1
                                   :message {:pdprofileid pdprofileid
                                             :nodeids [nodeid1]}}]]
                         @*logger-output*)))
                ;; Uncomment once DCC can handle Proximity Dimming messages.
                #_(is (= @msgpk-output
                         [(assoc-in node-profile
                                    [:nodeprops :nodeid]
                                    [nodeid])]))
                (is (= #{{:nodeid nodeid1}}
                       (nodes-for-profile pdprofileid)))
                (testing "Get-all should contain the new profile, with the node it's assigned to"
                  (is (= {:items [(assoc pdprofile
                                         :pdprofileid pdprofileid
                                         :sites []
                                         :groups []
                                         :nodes [{:nodeid nodeid1}])]
                          :success true}
                         (run get-all-profiles)))))

              ;; TODO: Uncomment once test fixtures are addressed.
              (testing "to a group"
                (binding [*logger-output* (atom [])]
                  (is (= {:result (assoc pdprofile
                                         :pdprofileid pdprofileid
                                         :groups [{:groupid groupid
                                                   :name "Test Lighting Group"}]
                                         :sites []
                                         :nodes [{:nodeid nodeid1}])
                          :success true}
                         (run (group-profile pdprofileid))))
                  (is (= [["user" {:userid "uberuser"
                                   :activity "applyPDtoGroup"
                                   :targettype "Group"
                                   :targetid groupid
                                   :message {:pdprofileid pdprofileid
                                             :siteid siteid
                                             :groupids [groupid]}}]
                          ["site" {:siteid siteid
                                   :userid "uberuser"
                                   :activity "applyPDtoGroup"
                                   :targettype "Group"
                                   :targetid groupid
                                   :message {:pdprofileid pdprofileid
                                             :siteid siteid
                                             :groupids [groupid]}}]
                          ["user" {:userid "uberuser"
                                   :activity "applyPDtoNodes"
                                   :targettype "Nodes"
                                   :targetid nodeid1
                                   :message {:pdprofileid pdprofileid
                                             :nodeids [nodeid1]}}]
                          ["site" {:siteid siteid
                                   :userid "uberuser"
                                   :activity "applyPDtoNodes"
                                   :targettype "Nodes"
                                   :targetid nodeid1
                                   :message {:pdprofileid pdprofileid
                                             :nodeids [nodeid1]}}]]
                         @*logger-output*)))
                ;; Uncomment once DCC can handle Proximity Dimming messages.
                #_(is (= @msgpk-output
                         [(assoc-in group-profile
                                    [:nodeprops :nodeid]
                                    []
                                    #_[nodeid])]))
                (is (= #{}
                       #_{:nodeid nodeid}
                       (nodes-for-profile "test-ubergroup-profile"))))

              (testing "to a site"
                (binding [*logger-output* (atom [])]
                  (is (= (-> {:result (assoc pdprofile
                                             :pdprofileid pdprofileid
                                             :groups [{:groupid groupid
                                                       :name "Test Lighting Group"}]
                                             :sites []
                                             :nodes [{:nodeid nodeid1}])
                              :error "Updating a site lighting group with a proximity dimming profile is not supported."
                              :success false}
                             vectors-to-sets)
                         (-> pdprofileid
                             site-profile
                             run
                             vectors-to-sets)))
                  (comment ; We no longer get this logging due to expected failure setting pd profile to site
                    (is (= [;; Site application
                          ["user" {:userid "uberuser"
                                   :activity "applyPDtoSite"
                                   :targettype "Site"
                                   :targetid siteid
                                   :message {:pdprofileid pdprofileid
                                             :siteid siteid}}]
                          ["site" {:siteid siteid
                                   :userid "uberuser"
                                   :activity "applyPDtoSite"
                                   :targettype "Site"
                                   :targetid siteid
                                   :message {:pdprofileid pdprofileid
                                             :siteid siteid}}]
                          ;; Application to nodes under the SLG
                          ["user" {:userid "uberuser"
                                   :activity "applyPDtoNodes"
                                   :targettype "Nodes"
                                   :targetid nodeid2
                                   :message {:pdprofileid pdprofileid
                                             :nodeids [nodeid2]}}]
                          ["site" {:siteid siteid
                                   :userid "uberuser"
                                   :activity "applyPDtoNodes"
                                   :targettype "Nodes"
                                   :targetid nodeid2
                                   :message {:pdprofileid pdprofileid
                                             :nodeids [nodeid2]}}]]
                         @*logger-output*))))
                ;; Uncomment once DCC can handle Proximity Dimming messages.
                #_(is (= @msgpk-output
                         [(assoc-in site-profile
                                    [:nodeprops :nodeid]
                                    [nodeid])]))
                (is (= (nodes-for-profile pdprofileid)
                       #{{:nodeid nodeid1}}))
                (testing "Get-all should contain the new profile, with the site and node it's assigned to"
                  (is (= (vectors-to-sets
                          {:items [(assoc pdprofile
                                          :pdprofileid pdprofileid
                                          :sites []
                                          :groups [{:name "Test Lighting Group"
                                                    :groupid groupid}]
                                          :nodes [{:nodeid nodeid1}])]
                           :success true})
                         (vectors-to-sets (run get-all-profiles)))))))))


        ;; Cleanup all created PDProfiles.
        (-> "MATCH (p:PDProfile) DETACH DELETE p"
            neo4j/executeQuery
            json/read-str)))))

(deftest test-photocell-schedule
  (let [orgid (create-test-org)
        siteid (create-test-site orgid)
        scheduleid (spy :debug (create-test-schedule orgid siteid {:events [{:days ["sat" "sun"]
                                                                           :photocell_enabled true
                                                                           :photocell_highLevel 100
                                                                           :photocell_lowLevel 0
                                                                           :actions [{:time "00:00:00"    :level 100}
                                                                                     {:time "08:00:00"    :level 0}]}
                                                                          {:days ["mon" "tue" "wed" "thu" "fri"]
                                                                           :photocell_enabled true
                                                                           :photocell_highLevel 80
                                                                           :photocell_lowLevel 20
                                                                           :actions []}]}))
        schedulePropsGet {:type      "getSchedule"
                          :user      "uberuser"
                          :orgprops  {:orgid orgid}
                          :siteprops {:siteid siteid}
                          :scheduleprops {:scheduleid scheduleid}}
        scheduleReceived (spy :debug (run schedulePropsGet))]
    (is (some? scheduleid))))

(deftest test-schedule-deletion
  (let [orgid (create-test-org)
        siteid (create-test-site orgid)
        site-scheduleid (->> {:type "getAllSchedules"
                              :user *user*
                              :orgprops {:orgid orgid}
                              :siteprops {:siteid siteid}}
                             run
                             :items
                             (map :scheduleid)
                             first)
        [schedule1 schedule2] (repeatedly #(create-test-schedule orgid siteid))]
    (is (some? schedule1))
    (is (some? schedule2))
    (is (= #{site-scheduleid
             schedule1 schedule2}
           (->> {:type "getAllSchedules"
                 :user *user*
                 :orgprops {:orgid orgid}
                 :siteprops {:siteid siteid}}
                run
                :items
                (map :scheduleid)
                set)))
    (is (= {:success true}
           (run {:type "deleteSchedule"
                 :user *user*
                 :orgprops {:orgid orgid}
                 :siteprops {:siteid siteid}
                 :scheduleprops {:scheduleid schedule1}})))
    (is (= #{site-scheduleid
             schedule2}
           (->> {:type "getAllSchedules"
                 :user *user*
                 :orgprops {:orgid orgid}
                 :siteprops {:siteid siteid}}
                run
                :items
                (map :scheduleid)
                set)))))

(deftest node-decommissioning-test
  (let [orgid (create-test-org)
        siteid (create-test-site orgid)
        nodeid (create-test-node orgid siteid)
        lg (create-test-lighting-group orgid
                                       siteid
                                       {:nodeList [nodeid]})
        scheduleid (create-test-schedule orgid siteid)
        etdhprofileid (create-test-etdhprofile orgid siteid)
        pdprofileid (create-test-pdprofile orgid siteid)]
    (let [{:keys [success]
           {:keys [groups
                   nodes]} :schedule} (run {:type "applyScheduleToGroup"
                                            :user *user*
                                            :orgprops {:orgid orgid}
                                            :siteprops {:siteid siteid}
                                            :scheduleprops {:scheduleid scheduleid}
                                            :groupprops {:groupids lg}})]
      (is (= true
             success))
      (is (= [lg]
             (map :groupid groups)))
      (is (= [nodeid]
             (map :nodeid nodes))))
    (let [{:keys [success]
           :as foo
           {:keys [groups
                   nodes]} :result} (run {:type "applyETDHtoGroup"
                                            :user *user*
                                            :orgprops {:orgid orgid}
                                            :siteprops {:siteid siteid}
                                            :etdhprofileprops {:etdhprofileid etdhprofileid}
                                            :groupprops {:groupids lg}})]
      (is (= true
             success))
      (is (= [lg]
             (map :groupid groups)))
      (is (= [nodeid]
             (map :nodeid nodes))))
    (let [{:keys [success]
           :as foo
           {:keys [groups
                   nodes]} :result} (run {:type "applyPDtoGroup"
                                            :user *user*
                                            :orgprops {:orgid orgid}
                                            :siteprops {:siteid siteid}
                                            :pdprofileprops {:pdprofileid pdprofileid}
                                            :groupprops {:groupids lg}})]
      (is (= true
             success))
      (is (= [lg]
             (map :groupid groups)))
      (is (= [nodeid]
             (map :nodeid nodes))))
    (let [invocation-log (atom [])]
      (with-redefs [pd/invalidate-pdprofile-cache-for-id #(swap! invocation-log conj [:invalidate-pd %&])
                    sch/send-loginreq-schedule #(swap! invocation-log conj [:schedule %&])
                    dctrl/query-exec-msgpk #(swap! invocation-log conj [:dctrl %&])]
        (let [{:keys [success]} (run {:type "deleteNode"
                                      :user *user*
                                      :orgprops {:orgid orgid}
                                      :siteprops {:siteid siteid}
                                      :nodeprops {:nodeid nodeid}})]
          (is (= true
                 success))))
      (is (= [[:invalidate-pd [pdprofileid]]
              [:schedule ["Unknown" "default" nodeid false "uberuser"]]
              [:dctrl [{:nodeprops {:type "LightingSetAuto"
                                    :nodeid nodeid}}]]]
             @invocation-log)))))

(deftest lighting-create-everything-test
  (testing "Creating a LG has default schedule assigned, and PD profile too."
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          scheduleid (create-test-schedule orgid siteid)
          pdprofileid (create-test-pdprofile orgid siteid)
          lighting-groupid (create-test-lighting-group orgid siteid {:schedules [{:scheduleid scheduleid}] :pdprofiles [{:pdprofileid pdprofileid}]})
          lighting-group (-> {:type "getGroup"
                              :user *user*
                              :orgprops {:orgid orgid}
                              :siteprops {:siteid siteid}
                              :groupprops {:groupid lighting-groupid}}
                             run
                             (doto (-> :success true? is))
                             :group)]
      (is (< 0 (count (:schedules lighting-group))))
      (is (= pdprofileid (:pdprofileid (first (:pdprofiles lighting-group))))))))

(deftest lighting-group-test
  (testing "Test Lighting Group CRUD operations"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          site-lighting-group (->> {:type "getAllGroups"
                                    :user *user*
                                    :orgprops {:orgid orgid}
                                    :siteprops {:siteid siteid}}
                                   run
                                   :items
                                   (map :groupid)
                                   first)
          [group1 group2] (testing "Creation"
                            (repeatedly #(create-test-lighting-group orgid siteid)))
          organizational-group (create-test-group orgid siteid)
          get-group-props #(-> {:type "getGroup"
                                :user *user*
                                :siteprops {:siteid siteid}
                                :groupprops {:groupid %}}
                               run
                               (doto
                                   (-> :success
                                       assert))
                               :group)]
      (is (some? site-lighting-group))
      (is (some? group1))
      (is (some? group2))
      (is (= 3
             (count #{site-lighting-group
                      group1 group2})))

      (testing "Creation fails for site lighting groups"
        (is (= {:error "Site lighting groups cannot be manually created."}
               (-> {:type "createGroup"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :groupprops {:groupid (uuid)
                                 :name "manually created site lighting group"
                                 :nodeList []
                                 :type "site-lighting"}}
                   run))))

      (testing "Get-All"
        (let [get-all-groups (->> {:type "getAllGroups"
                                   :user *user*
                                   :siteprops {:siteid siteid}}
                                  run
                                  :items)]
          (testing "returns site lighting group"
            (is (= [site-lighting-group]
                   (->> get-all-groups
                        (filter (comp #{"site-lighting"}
                                      :type))
                        (map :groupid)))))

          (testing "returns lighting groups"
            (is (= #{group1 group2}
                   (->> get-all-groups
                        (filter (comp #{"lighting"}
                                      :type))
                        (map :groupid)
                        set))))

          (testing "returns organizational groups"
            (is (= [organizational-group]
                   (->> get-all-groups
                        (filter (comp #{"organizational"}
                                      :type))
                        (map :groupid)))))))

      (testing "Get"
        (testing "returns site lighting group"
          (is (= {:groupid site-lighting-group
                  :type "site-lighting"}
                 (-> (get-group-props site-lighting-group)
                     (select-keys [:groupid :type])))))

        (testing "returns lighting groups"
          (is (= {:groupid group1
                  :type "lighting"}
                 (-> (get-group-props group1)
                     (select-keys [:groupid :type])))))

        (testing "returns organizational groups"
          (is (= {:groupid organizational-group
                  :type "organizational"}
                 (-> (get-group-props organizational-group)
                     (select-keys [:groupid :type]))))))

      (testing "Updating"
        (let [lgprops (-> (get-group-props group1)
                          (assoc :groupid group1
                                 :type "organizational"))
              ogprops (-> (get-group-props organizational-group)
                          (assoc :groupid organizational-group
                                 :type "lighting"))
              slgprops #(-> (get-group-props site-lighting-group)
                            (assoc :groupid site-lighting-group))]

          (testing "fails when changing the group type"
            (is (= {:error "Updating a group with a different type is not supported."}
                   (run {:type "updateGroup"
                         :user *user*
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :groupprops lgprops})))
            (is (= {:error "Updating a group with a different type is not supported."}
                   (run {:type "updateGroup"
                         :user *user*
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :groupprops ogprops}))))
          (testing "succeeds when keeping the group type the same"
            (is (= {:success true
                    :group {:groupid group2
                            :name "name"
                            :description "desc"
                            :nodeList []
                            :nodes [{}]
                            :type "lighting"}}
                   (-> {:type "updateGroup"
                        :user *user*
                        :orgprops {:orgid orgid}
                        :siteprops {:siteid siteid}
                        :groupprops {:groupid group2
                                     :name "name"
                                     :description "desc"
                                     :nodeList []
                                     :nodes [{}]
                                     :type "lighting"}}
                       run
                       (update :group
                               dissoc
                               :etdhprofiles :dhprofiles :pdprofiles :schedules)))))
          (testing "fails when altering site lighting group type"
            (is (= {:error "Site lighting group type cannot be changed."}
                   (-> {:type "updateGroup"
                        :user *user*
                        :orgprops {:orgid orgid}
                        :siteprops {:siteid siteid}
                        :groupprops (assoc (slgprops)
                                           :type "lighting")}
                       run))))
          (testing "fails when altering site lighting group node membership"
            (let [nodeid (create-test-node orgid siteid)]
              (is (= {:error "Node can not be removed from Site Lighting group."}
                     (-> {:type "updateGroup"
                          :user *user*
                          :orgprops {:orgid orgid}
                          :siteprops {:siteid siteid}
                          :groupprops (assoc (slgprops)
                                             :nodeList [])}
                         run)))))
          (testing "succeeds when altering other site lighting group name/description"
            (is (true? (-> {:type "updateGroup"
                            :user *user*
                            :orgprops {:orgid orgid}
                            :siteprops {:siteid siteid}
                            :groupprops (-> (slgprops)
                                            (update :name str "x")
                                            (update :description str "x"))}
                           run
                           :success))))))

      (testing "Deletion"
        (let [nodeid (create-test-node orgid siteid)
              lgprops (-> (get-group-props group1)
                          (assoc :groupid group1
                                 :nodeList [nodeid]))]
          (is (true? (-> {:type "updateGroup"
                          :user *user*
                          :orgprops {:orgid orgid}
                          :siteprops {:siteid siteid}
                          :groupprops lgprops}
                         run
                         :success)))
          (testing "fails when the lighting group has nodes"
            (is (= {:error "Lighting Group can't be deleted when it has nodes. Please move the nodes to a different group before deleting this lighting group."}
                   (run {:type "deleteGroup"
                         :user *user*
                         :siteprops {:siteid siteid}
                         :orgprops {:orgid orgid}
                         :groupprops {:groupid group1}}))))
          (testing "succeeds when the lighting group has no nodes"
            (is (true? (-> {:type "deleteGroup"
                            :user *user*
                            :siteprops {:siteid siteid}
                            :orgprops {:orgid orgid}
                            :groupprops {:groupid group2}}
                           run
                           :success))))
          (testing "fails when attempting to remove the site lighting group"
            (is (= {:error "A site lighting group cannot be deleted."}
                   (run {:type "deleteGroup"
                         :user *user*
                         :siteprops {:siteid siteid}
                         :orgprops {:orgid orgid}
                         :groupprops {:groupid site-lighting-group}}))))))))
  (testing "Test Lighting Group Node membership"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          site-lighting-groupid (->> {:type "getAllGroups"
                                      :user *user*
                                      :orgprops {:orgid orgid}
                                      :siteprops {:siteid siteid}}
                                     run
                                     :items
                                     (map :groupid)
                                     first)
          [nodeid1 nodeid2] (repeatedly #(create-test-node orgid siteid))
          [group1 group2] (testing "Creation"
                            (repeatedly #(create-test-lighting-group orgid siteid)))
          organizational-group (create-test-group orgid siteid {:nodeList [nodeid1]})
          get-group-props #(-> {:type "getGroup"
                                :user *user*
                                :siteprops {:siteid siteid}
                                :groupprops {:groupid %}}
                               run
                               (doto
                                   (-> :success
                                       assert))
                               :group
                               (dissoc :dhprofiles
                                       :pdprofiles
                                       :schedules))]
      (testing "when nodes are moved between lighting groups"
        (let [lgprops1 (-> (get-group-props group1)
                           (assoc :groupid group1
                                  :nodeList [nodeid1 nodeid2]))
              lgprops2 (-> (get-group-props group2)
                           (assoc :groupid group2
                                  :nodeList [nodeid1 nodeid2]))]
          (is (= [nodeid1]
                 (:nodeList (get-group-props organizational-group))))
          (is (true? (-> {:type "updateGroup"
                          :user *user*
                          :orgprops {:orgid orgid}
                          :siteprops {:siteid siteid}
                          :groupprops lgprops1}
                         run
                         :success)))
          (is (= [nodeid1]
                 (:nodeList (get-group-props organizational-group))))
          (is (true? (-> {:type "updateGroup"
                          :user *user*
                          :orgprops {:orgid orgid}
                          :siteprops {:siteid siteid}
                          :groupprops lgprops2}
                         run
                         :success)))
          (is (= [nodeid1]
                 (:nodeList (get-group-props organizational-group))))

          (is (= []
                 (:nodeList (get-group-props group1))))
          (is (= #{nodeid1 nodeid2}
                 (-> (get-group-props group2)
                     :nodeList
                     set)))
          (testing "Using `addNodeToGroup` on LG"
            (is (true? (-> {:type "addNodeToGroup"
                            :user *user*
                            :orgprops {:orgid orgid}
                            :siteprops {:siteid siteid}
                            :groupprops {:groupid group1}
                            :nodeprops {:nodeid nodeid1}}
                           run
                           :success)))
            (is (= [nodeid1]
                   (:nodeList (get-group-props group1)))))
          (testing "Using `removeNodeFromGroup` on LG"
            (is (true? (-> {:type "removeNodeFromGroup"
                            :user *user*
                            :orgprops {:orgid orgid}
                            :siteprops {:siteid siteid}
                            :groupprops {:groupid group2}
                            :nodeprops {:nodeid nodeid2}}
                           run
                           :success)))
            (is (= []
                   (:nodeList (get-group-props group2))))
            (is (= [nodeid2]
                   (:nodeList (get-group-props site-lighting-groupid)))))
          (testing "Using `addNodeToGroup` on SLG"
            (is (true? (-> {:type "addNodeToGroup"
                            :user *user*
                            :orgprops {:orgid orgid}
                            :siteprops {:siteid siteid}
                            :groupprops {:groupid site-lighting-groupid}
                            :nodeprops {:nodeid nodeid1}}
                           run
                           :success)))
            (is (= #{nodeid1 nodeid2}
                   (-> (get-group-props site-lighting-groupid)
                       :nodeList
                       set))))
          (testing "Using `removeNodeFromGroup` on SLG should fail"
            (is (= {:error "Node can not be removed from Site Lighting group"
                    :success false}
                   (run {:type "removeNodeFromGroup"
                         :user *user*
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :groupprops {:groupid site-lighting-groupid}
                         :nodeprops {:nodeid nodeid2}})))
            (is (= #{nodeid1 nodeid2}
                   (-> (get-group-props site-lighting-groupid)
                       :nodeList
                       set))))))))
  (testing "Lighting groups act on scheduleid passed in update."
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          site-lighting-groupid (->> {:type "getAllGroups"
                                      :user *user*
                                      :orgprops {:orgid orgid}
                                      :siteprops {:siteid siteid}}
                                     run
                                     :items
                                     (map :groupid)
                                     first)
          [nodeid1 nodeid2 nodeid3] (repeatedly #(create-test-node orgid siteid))
          [scheduleid1 scheduleid2] (repeatedly #(create-test-schedule orgid siteid))
          lighting-groupid (create-test-lighting-group orgid siteid)
          lighting-group (-> {:type "getGroup"
                              :user *user*
                              :orgprops {:orgid orgid}
                              :siteprops {:siteid siteid}
                              :groupprops {:groupid lighting-groupid}}
                             run
                             (doto (-> :success true? is))
                             :group)]
      (testing "Adding a schedule to the LG"
        (is (= (-> lighting-group
                   (assoc :schedules [{:scheduleid scheduleid1
                                       :name "Test Schedule"}])
                   vectors-to-sets)
               (-> {:type "updateGroup"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :groupprops (-> lighting-group
                                    (assoc :schedules [{:scheduleid scheduleid1}]))}
                   run
                   (doto (-> :success
                             true?
                             is))
                   :group
                   vectors-to-sets))))
      (testing "Changing the assigned LG schedule"
        (is (= (-> lighting-group
                   (assoc :schedules [{:scheduleid scheduleid2
                                       :name "Test Schedule"}])
                   vectors-to-sets)
               (-> {:type "updateGroup"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :groupprops (-> lighting-group
                                    (assoc :schedules [{:scheduleid scheduleid2}]))}
                   run
                   (doto (-> :success
                             true?
                             is))
                   :group
                   vectors-to-sets))))
      (testing "deleting the assigned lg schedule"
        (is (= (-> lighting-group
                   (assoc :schedules [])
                   vectors-to-sets)
               (-> {:type "updateGroup"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :groupprops (-> lighting-group
                                    (assoc :schedules []))}
                   run
                   (doto (-> :success
                             true?
                             is))
                   :group
                   vectors-to-sets))))
      (is (= (-> lighting-group
                 (assoc :schedules [{:scheduleid scheduleid1
                                     :name "Test Schedule"}])
                 vectors-to-sets)
             (-> {:type "updateGroup"
                  :user *user*
                  :orgprops {:orgid orgid}
                  :siteprops {:siteid siteid}
                  :groupprops (-> lighting-group
                                  (assoc :schedules [{:scheduleid scheduleid1}]))}
                 run
                 (doto (-> :success
                           true?
                           is))
                 :group
                 vectors-to-sets)))
      (testing "Deleting a schedule assigned to the LG"
        (is (= {:error "Cannot delete schedule in use by a Lighting Group."}
               (run {:type "deleteSchedule"
                     :user *user*
                     :orgprops {:orgid orgid}
                     :siteprops {:siteid siteid}
                     :scheduleprops {:scheduleid scheduleid1}})))
        (is (= (-> lighting-group
                   (assoc :schedules [{:scheduleid scheduleid1
                                       :name "Test Schedule"}])
                   vectors-to-sets)
               (-> {:type "getGroup"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :groupprops {:groupid lighting-groupid}}
                   run
                   (doto (-> :success
                             true?
                             is))
                   :group
                   vectors-to-sets))))

      (is (true? (-> {:type "updateGroup"
                      :user *user*
                      :orgprops {:orgid orgid}
                      :siteprops {:siteid siteid}
                      :groupprops lighting-group}
                     run
                     :success)))
      (is (= #{nodeid1 nodeid2 nodeid3}
             (-> {:type "getGroup"
                  :user *user*
                  :orgprops {:orgid orgid}
                  :siteprops {:siteid siteid}
                  :groupprops {:groupid site-lighting-groupid}}
                 run
                 (doto (-> :success true? is))
                 :group
                 :nodeList
                 set)))))
  (testing "Can apply schedules to multiple groups"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          slg-scheduleid (->> {:type "getAllSchedules"
                               :user *user*
                               :orgprops {:orgid orgid}
                               :siteprops {:siteid siteid}}
                              run
                              :items
                              (filter (comp #{"Default schedule"}
                                            :name))
                              (map :scheduleid)
                              first)
          [nodeid1 nodeid2 nodeid3 nodeid4] (repeatedly #(create-test-node orgid siteid))
          [groupid1 groupid2] (->> [nodeid1 nodeid2]
                                   (map #(create-test-lighting-group orgid
                                                                     siteid
                                                                     {:nodeList [%]})))
          groupid3 (create-test-lighting-group orgid siteid)
          groupid4 (create-test-lighting-group orgid siteid {:nodeList [nodeid3 nodeid4]})
          schedule {:events [{:days ["mon" "tue" "wed" "thu" "fri" "sat" "sun"]
                              :photocell_enabled false
                              :photocell_highLevel 100
                              :photocell_lowLevel 0
                              :actions [{:level 100
                                         :time "00:00:00"}]}]}
          scheduleid (create-test-schedule orgid siteid schedule)]
      (is (= (-> {:schedule (assoc schedule
                                   :scheduleid scheduleid
                                   :name "Test Schedule"
                                   :description "This is a test schedule"
                                   :network default-test-no-network-schedule
                                   :sites []
                                   :groups [{:groupid groupid1
                                             :name "Test Lighting Group"}
                                            {:groupid groupid2
                                             :name "Test Lighting Group"}
                                            {:groupid groupid3
                                             :name "Test Lighting Group"}
                                            {:groupid groupid4
                                             :name "Test Lighting Group"}]
                                   :nodes [{:nodeid nodeid1}
                                           {:nodeid nodeid2}
                                           {:nodeid nodeid3}
                                           {:nodeid nodeid4}])
                  :success true}
                 vectors-to-sets)
             (-> {:type "applyScheduleToGroup"
                  :user *user*
                  :orgprops {:orgid orgid}
                  :siteprops {:siteid siteid}
                  :scheduleprops {:scheduleid scheduleid}
                  :groupprops {:groupids [groupid1 groupid2 groupid3 groupid4]}}
                 run
                 vectors-to-sets)))
      (is (= (-> {:schedule {:nodes [{:nodeid nodeid1}
                                     {:nodeid nodeid2}
                                     {:nodeid nodeid3}
                                     {:nodeid nodeid4}]
                             :name "Test Schedule"
                             :description "This is a test schedule"
                             :groups [{:groupid groupid1
                                       :name "Test Lighting Group"}
                                      {:groupid groupid2
                                       :name "Test Lighting Group"}
                                      {:groupid groupid3
                                       :name "Test Lighting Group"}
                                      {:groupid groupid4
                                       :name "Test Lighting Group"}]
                             :sites []
                             :scheduleid scheduleid}
                  :success true}
                 vectors-to-sets)
             (-> {:type "getSchedule"
                  :user *user*
                  :orgprops {:orgid orgid}
                  :siteprops {:siteid siteid}
                  :scheduleprops {:scheduleid scheduleid}}
                 run
                 (update :schedule dissoc
                         :events
                         :network)
                 vectors-to-sets)))
      (let [new-lg (create-test-lighting-group orgid siteid {:nodeList [nodeid2]})]
        (is (= slg-scheduleid
               (-> {:type "getNode"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :nodeprops {:nodeid nodeid2}}
                   run
                   (doto (-> :success
                             true?
                             is))
                   :node
                   :scheduleid)))
        (is (= {:success true
                :result {:groupid new-lg
                         :nodeids [nodeid3]
                         :siteid siteid}}
               (-> {:type "addNodeToGroup"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :groupprops {:groupid new-lg}
                    :nodeprops {:nodeid nodeid3}}
                   run)))
        (is (= slg-scheduleid
               (-> {:type "getNode"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :nodeprops {:nodeid nodeid3}}
                   run
                   (doto (-> :success
                             true?
                             is))
                   :node
                   :scheduleid)))
        (is (= true
               (-> {:type "updateGroup"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :groupprops (-> {:type "getGroup"
                                     :user *user*
                                     :orgprops {:orgid orgid}
                                     :siteprops {:siteid siteid}
                                     :groupprops {:groupid new-lg}}
                                    run
                                    (doto (-> :success true? is))
                                    :group
                                    (update :nodeList
                                            conj nodeid4))}
                   run
                   :success)))
        (is (= slg-scheduleid
               (-> {:type "getNode"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :nodeprops {:nodeid nodeid4}}
                   run
                   (doto (-> :success
                             true?
                             is))
                   :node
                   :scheduleid))))))
  (testing "Updating the SLG should update nodes in LGs without specified Schedules/Profiles"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          slg-groupid (->> {:type "getAllGroups"
                            :user *user*
                            :orgprops {:orgid orgid}
                            :siteprops {:siteid siteid}}
                           run
                           :items
                           (map :groupid)
                           first)
          slg-scheduleid (->> {:type "getAllSchedules"
                               :user *user*
                               :orgprops {:orgid orgid}
                               :siteprops {:siteid siteid}}
                              run
                              :items
                              (filter (comp #{"Default schedule"}
                                            :name))
                              (map :scheduleid)
                              first)
          [nodeid1 nodeid2] (repeatedly #(create-test-node orgid siteid))
          lg-groupid (create-test-lighting-group orgid siteid {:nodeList [nodeid1]})
          [scheduleid1 scheduleid2] (repeatedly #(create-test-schedule orgid siteid))]
      (is (= []
             (-> {:type "getGroup"
                  :user *user*
                  :orgprops {:orgid orgid}
                  :siteprops {:siteid siteid}
                  :groupprops {:groupid lg-groupid}}
                 run
                 (doto (-> :success true? is))
                 :group
                 :schedules)))
      (is (= (-> {:success true
                  :schedule (assoc default-test-schedule
                                   :scheduleid scheduleid1
                                   :sites [{:groupid slg-groupid
                                            :name "Site Lighting Group"}]
                                   :groups []
                                   :nodes [{:nodeid nodeid1}
                                           {:nodeid nodeid2}])}
                 vectors-to-sets)
             (-> {:type "applyScheduleToGroup"
                  :user *user*
                  :orgprops {:orgid orgid}
                  :siteprops {:siteid siteid}
                  :scheduleprops {:scheduleid scheduleid1}
                  :groupprops {:groupids [slg-groupid]}}
                 run
                 vectors-to-sets)))
      (is (= scheduleid1
             (-> {:type "getNode"
                  :user *user*
                  :orgprops {:orgid orgid}
                  :siteprops {:siteid siteid}
                  :nodeprops {:nodeid nodeid1}}
                 run
                 (doto (-> :success
                           true?
                           is))
                 :node
                 :scheduleid)))
      (is (true? (-> {:type "updateGroup"
                      :user *user*
                      :orgprops {:orgid orgid}
                      :siteprops {:siteid siteid}
                      :groupprops (-> {:type "getGroup"
                                       :user *user*
                                       :orgprops {:orgid orgid}
                                       :siteprops {:siteid siteid}
                                       :groupprops {:groupid slg-groupid}}
                                      run
                                      (doto (-> :success true? is))
                                      :group
                                      (assoc :schedules [{:scheduleid scheduleid2}]))}
                     run
                     :success)))
      (is (= scheduleid2
             (-> {:type "getNode"
                  :user *user*
                  :orgprops {:orgid orgid}
                  :siteprops {:siteid siteid}
                  :nodeprops {:nodeid nodeid1}}
                 run
                 (doto (-> :success
                           true?
                           is))
                 :node
                 :scheduleid))))))

(deftest site-lighting-group-test
  (testing "Site Lighting Groups"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          [{slg-name :name
            slg-id :groupid
            :as site-group}
           :as all-groups] (-> {:type "getAllGroups"
                                :user *user*
                                :orgprops {:orgid orgid}
                                :siteprops {:siteid siteid}}
                               run
                               (doto (-> :success
                                         true?
                                         is))
                               :items)
          [{sch-name :name
            sch-id :scheduleid
            :as site-schedule}
           :as all-schedules] (-> {:type "getAllSchedules"
                                   :user *user*
                                   :orgprops {:orgid orgid}
                                   :siteprops {:siteid siteid}}
                                  run
                                  (doto (-> :success
                                            true?
                                            is))
                                  :items)]
      (testing "are created, along with a single schedule, upon site creation"
        (is (= 1
               (count all-groups)
               (count all-schedules))))
      (is (= "Default schedule"
             sch-name))
      (is (= "Site Lighting Group"
             slg-name))
      (is (= [{:name sch-name
               :scheduleid sch-id}]
             (:schedules site-group)))
      (is (= [{:name slg-name
               :groupid slg-id}]
             (:sites site-schedule)))
      (testing "will not allow its assigned schedule to be deleted"
        (is (= {:error "Cannot delete schedule in use by Site Lighting Group."}
               (run {:type "deleteSchedule"
                     :user *user*
                     :orgprops {:orgid orgid}
                     :siteprops {:siteid siteid}
                     :scheduleprops {:scheduleid sch-id}}))))
      (testing "will not allow its schedule to be unassigned"
        (is (= {:error "Site lighting group must be assigned schedule."}
               (-> {:type "updateGroup"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :groupprops  (assoc site-group
                                        :schedules [])}
                   run))))
      (testing "control nodes as expected"
        (let [nodeid (create-test-node orgid siteid)
              [node] (-> {:type "getAllNodesForSite"
                          :user *user*
                          :orgprops {:orgid orgid}
                          :siteprops {:siteid siteid}}
                         run
                         (doto (-> :success
                                   true?
                                   is))
                         :items)]
          (is (= {:scheduleid sch-id
                  :schedulename sch-name
                  :groupidlist [slg-id]
                  :groupnamelist [slg-name]}
                 (select-keys node
                              [:groupidlist
                               :groupnamelist
                               :scheduleid
                               :schedulename]))))))))
