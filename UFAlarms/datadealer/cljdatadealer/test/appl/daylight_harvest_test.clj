(ns appl.daylight-harvest-test
  (:refer-clojure :exclude [iterate])
  (:require [appl.daylight_harvest
             :as dh
             :refer :all]
            [clj-time.core :as time-core]
            [clojure
             [test :refer :all]
             [walk :refer [keywordize-keys]]]
            [clojure.data.json :as json]
            [dealer
             [devsvcctrl :as dev-ctrl]
             [devsvcctrl-test :as dev-ctrl-test]]
            [mqtt.fixture :as mqtt]
            [kafka.mock :as kafka]
            [neowrap.neowrapper :as neo4j]
            [utils
             [cape-test :refer [vectors-to-sets
                                run
                                uuid
                                create-test-org
                                create-test-site
                                default-test-site
                                create-test-node
                                create-test-lighting-group
                                create-test-dhprofile
                                default-test-dhprofile]]
             [cassandra_fixture :as cass]
             [neo4j-fixture :as neo4j-fixture]]
            [utils.async.async_chans :as ac])
  (:import org.joda.time.DateTime))

(use-fixtures :once (join-fixtures [cass/cassandra-fixture
                                    neo4j-fixture/neo4j-fixture
                                    kafka/kafka-fixture
                                    mqtt/mqtt-fixture
                                    dev-ctrl-test/device-service-control-fixture
                                    utils.cape-test/mock-google-api]))

(def iterate-config
  {:gain 0.5
   :reset-time 8
   :max-drive 100.0
   :min-drive 1.0
   :set-point 100
   :slew-rate 0.5})

(deftest iterate-test
  (testing "Iterative PI Controller"
    (is (= (iterate iterate-config
                    25
                    0
                    0)
           [42.1875 3/4 42.1875]))
    (is (= (iterate iterate-config
                    30
                    42.1875
                    3/4)
           [81.1875 7/10 39.0]))))

(def user utils.cape-test/*user*)

;; This test is to show how consecutive sensor readings will affect
;; output values due to previous output and error.
(deftest record-ambient-light-test
  (testing "Samples from nodes not associated with a DHProfile should be a no-op"
    (with-redefs [ambient-light-values (atom {})]
      ;; "DNE" does not exist, and therefore, cannot have an
      ;; associated DHProfile.
      (is (= nil
             (record-ambient-light "DNE" 12)))
      (is (empty? @ambient-light-values))))
  (testing "Send sensor sample data"
    (try
      (let [orgid (create-test-org)
            siteid (create-test-site orgid)
            nodeid (create-test-node orgid siteid)
            dhprofileid (create-test-dhprofile orgid siteid default-test-dhprofile)]
        ;; Apply Profile to Node
        (is (= {:result (-> default-test-dhprofile
                            (assoc :dhprofileid dhprofileid
                                   :autocalibrate true
                                   :nodes [{:nodeid nodeid}]
                                   :groups []
                                   :sites []))
                :success true}
               (run {:type "applyDHtoNodes"
                        :user user
                        :orgprops {:orgid orgid}
                        :siteprops {:siteid siteid}
                        :dhprofileprops {:dhprofileid dhprofileid}
                        :nodeprops {:nodeids [nodeid]}})))
          (is (nil? (-> "MATCH (p:DHProfile {dhprofileid: {props}.dhprofileid})
SET p.setPoint = {props}.setpoint"
                        (neo4j/executeQuery {"dhprofileid" dhprofileid
                                             "setpoint" (:setPoint default-test-dhprofile)})
                        (json/read-str :key-fn keyword)
                        :exception)))

        (with-redefs [ambient-light-values (atom {})
                      group-state (atom {})]
          (is (= {nodeid 25}
                 (record-ambient-light nodeid 25000)
                 @ambient-light-values))
          (is (= {:nodeid [nodeid]
                  :name "LightingForceState"
                  :pri 4
                  :mask 1
                  :level 42
                  :qualifiers "undefined"
                  :ftype "Volatile"}
                 (-> dhprofileid
                     get-dhprofile
                     run-controller
                     keywordize-keys)))
          (is (= {dhprofileid {:error 0.75
                               :out 42
                               :deltas (update-deltas nil 42.1875)}}
                 @group-state))

          (is (= {nodeid 30}
                 (record-ambient-light nodeid 30000)
                 @ambient-light-values))
          (is (= {:nodeid [nodeid]
                  :name "LightingForceState"
                  :pri 4
                  :mask 1
                  :level 81
                  :qualifiers "undefined"
                  :ftype "Volatile"}
                 (-> dhprofileid
                     get-dhprofile
                     run-controller
                     keywordize-keys)))
          (is (= {dhprofileid {:error 0.7
                               :out 81
                               :deltas (-> nil
                                           (update-deltas 42.1875)
                                           (update-deltas 39.0))}}
                 @group-state))


          (is (= {nodeid 151}
                 (record-ambient-light nodeid 151000)
                 @ambient-light-values))
          (is (= {:nodeid [nodeid]
                  :name "LightingForceState"
                  :pri 4
                  :mask 1
                  :level (:minDrive default-test-dhprofile)
                  :qualifiers "undefined"
                  :ftype "Volatile"}
                 (-> dhprofileid
                     get-dhprofile
                     run-controller
                     keywordize-keys)))))
      (finally
        (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;")))))

(def nodes
  (->> [[37.36811534 -122.03757588]
        [37.36983242 -122.03389716]
        [37.36679022 -122.0341788]
        [37.36934578 -122.03476659]
        [37.36996036 -122.03593197]
        [37.36992513 -122.03861171]
        [37.36992311 -122.03938024]
        [37.36964603 -122.0336734]
        [37.36796548 -122.03585974]
        [37.36870227 -122.03342999]
        [37.36867506 -122.03847063]
        [37.36825062 -122.03652623]
        [37.36681808 -122.03868661]
        [37.37096703 -122.03801712]
        [37.36826681 -122.03648771]
        [37.37162073 -122.0366395]
        [37.36934747 -122.0352925]
        [37.36821861 -122.03465042]
        [37.36916776 -122.03821233]
        [37.36994171 -122.03310055]
        [37.36947881 -122.0390822]
        [37.36900883 -122.03286922]
        [37.36962951 -122.03493443]
        [37.36915387 -122.03850341]
        [37.37122611 -122.03824216]]
       (map (fn [[latitude longitude]]
              {:latitude (str latitude)
               :longitude (str longitude)}))))

(deftest test-daylight-harvesting
  (try
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          nodeList (map #(create-test-node orgid siteid %)
                        nodes)
          nodes (map #(assoc %1 :nodeid %2) nodes nodeList)

          groupid (create-test-lighting-group orgid siteid {:nodeList nodeList})
          nodeid (first nodeList)
          sensor "l"

          dhprofileid (create-test-dhprofile orgid siteid default-test-dhprofile)

          apply-dh {:dhprofileprops {:dhprofileid dhprofileid}
                    :orgprops {:orgid orgid}
                    :type "applyDHtoGroup"
                    :nodeprops {:type "DaylightHarvesting"}
                    :user user
                    :groupprops {:groupids [groupid]}
                    :siteprops {:siteid siteid}}

          get-trigger-nodes {:type "getAllDHProfileTriggers"
                             :user user
                             :orgprops {:orgid orgid}
                             :siteprops {:siteid siteid}
                             :dhprofileprops {:dhprofileid dhprofileid}}

          remove-trigger-node #(do {:type "removeDHProfileTrigger"
                                    :user user
                                    :orgprops {:orgid orgid}
                                    :siteprops {:siteid siteid}
                                    :dhprofileprops {:dhprofileid %1}
                                    :nodeprops {:nodeid %2}})

          add-trigger-node #(do {:type "addDHProfileTrigger"
                                 :user user
                                 :orgprops {:orgid orgid}
                                 :siteprops {:siteid siteid}
                                 :dhprofileprops {:dhprofileid %1}
                                 :nodeprops {:nodeid %2}})

          send-sensor-sample (fn [value]
                               (ac/put-sensor-sample {:nodeid nodeid
                                                      :sensor sensor
                                                      :value 0.0
                                                      :time 0})
                               (-> dhprofileid
                                   get-dhprofile
                                   run-controller))

          msgpk-output (atom nil)]

      (with-redefs [dev-ctrl/query-exec-msgpk #(reset! msgpk-output %)
                    ac/put-sensor-sample (fn [{:keys [nodeid value]}]
                                           (record-ambient-light nodeid value))]

        (testing "Removing trigger nodes should fail when a DHProfile does not exist"
          (is (= {:success false
                  :error "Could not find dhprofile with trigger node."}
                 (run (remove-trigger-node "DNE" nodeid)))))

        (testing "Adding trigger nodes should fail when a DHProfile does not exist"
          (is (= {:success false
                  :error "Could not find dhprofile with trigger node."}
                 (run (add-trigger-node "DNE" nodeid)))))

        (testing "sensor samples from a node without a DHProfile have no effect"
          (send-sensor-sample 0.0)
          (is (= @msgpk-output
                 nil)))

        (testing "An unassigned DHProfile should have no trigger nodes"
          (is (= {:items []
                  :success true}
                 (run get-trigger-nodes))))

        (testing "Removing trigger nodes should fail when a DHProfile has no nodes."
          (is (= {:success false
                  :error "Could not find dhprofile with trigger node."}
                 (run (remove-trigger-node dhprofileid nodeid)))))

        (testing "Adding trigger nodes should fail when a DHProfile has no nodes."
          (is (= {:success false
                  :error "Could not find dhprofile with trigger node."}
                 (run (add-trigger-node dhprofileid nodeid)))))

        (testing "sensor samples from a DHTrigger have consequences"
          (is (= (vectors-to-sets
                  {:result (-> default-test-dhprofile
                               (assoc :dhprofileid dhprofileid
                                      :autocalibrate true
                                      :nodes (mapv #(do {:nodeid %}) nodeList)
                                      :groups [{:groupid groupid
                                                :name "Test Lighting Group"}]
                                      :sites []))
                   :success true})
                 (vectors-to-sets (run apply-dh))))
          (is (nil? (-> "MATCH (p:DHProfile {dhprofileid: {props}.dhprofileid})
SET p.setPoint = {props}.setpoint"
                        (neo4j/executeQuery {"dhprofileid" dhprofileid
                                             "setpoint" (:setPoint default-test-dhprofile)})
                        (json/read-str :key-fn keyword)
                        :exception)))
          (send-sensor-sample 0.0)
          (is (= {:nodeprops {:type "LightingForceState"
                              :nodeid (set nodeList)
                              :pri 4
                              :level 56
                              :timeout 0}}
                 (update-in @msgpk-output [:nodeprops :nodeid] set))))

        (testing "A assigned DHProfile should, initially, have trigger nodes"
          (is (= {:items (set nodes)
                  :success true}
                 (update (run get-trigger-nodes)
                         :items
                         set))))

        (testing "Removing trigger nodes should fail when not in the DHProfile."
          (is (= {:success false
                  :error "Could not find dhprofile with trigger node."}
                 (run (remove-trigger-node dhprofileid "DNE")))))

        (testing "Successfully removing trigger node from a DHProfile."
          (is (= {:items (set nodes)
                  :success true}
                 (update (run get-trigger-nodes)
                         :items
                         set)))
          (is (= {:success true}
                 (run (remove-trigger-node dhprofileid nodeid))))
          (is (= {:items (->> nodes
                              (remove (comp #(= % nodeid)
                                            :nodeid))
                              set)
                  :success true}
                 (update (run get-trigger-nodes)
                         :items
                         set))))

        (testing "A assigned DHProfile should have less trigger nodes after they are removed"
          (is (= {:items (->> nodes
                              (remove (comp #(= % nodeid)
                                            :nodeid))
                              set)
                  :success true}
                 (update (run get-trigger-nodes)
                         :items
                         set))))

        (testing "Successfully adding trigger node from a DHProfile."
          (is (= {:items (->> nodes
                              (remove (comp #(= % nodeid)
                                            :nodeid))
                              set)
                  :success true}
                 (update (run get-trigger-nodes)
                         :items
                         set)))
          (is (= {:success true}
                 (run (add-trigger-node dhprofileid nodeid))))
          (is (= {:items (set nodes)
                  :success true}
                 (update (run get-trigger-nodes)
                         :items
                         set))))

        (testing "A assigned DHProfile should have more trigger nodes after they are added back"
          (is (= {:items (set nodes)
                  :success true}
                 (update (run get-trigger-nodes)
                         :items
                         set))))))
    (finally
      (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;"))))

(deftest calibration-test
  (let [cape-calibrate #(run {:type "calibrateDHProfile"
                              :user user
                              :siteprops {:siteid :unused}
                              :orgprops {:orgid :unused}
                              :dhprofileprops {:dhprofileid %}})]
    (testing "Fail when no DHProfile exists"
      (is (= {:success false
              :error {:message "Cannot calibrate DHProfile with no Nodes."
                      :data {:dhprofileid "DNE"}}}
             (update (cape-calibrate "DNE")
                     :error json/read-str :key-fn keyword))))
    (testing "Fail when there are no controller nodes"
      (try
        (let [orgid (create-test-org)
              siteid (create-test-site orgid)
              dhprofileid (create-test-dhprofile orgid siteid default-test-dhprofile)]
          (is (= {:success false
                  :error {:message "Cannot calibrate DHProfile with no Nodes."
                          :data {:dhprofileid dhprofileid}}}
                 (update (cape-calibrate dhprofileid)
                         :error json/read-str :key-fn keyword))))
        (finally
          (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;"))))
    (testing "Fail when there are no trigger nodes"
      (try
        (let [orgid (create-test-org)
              siteid (create-test-site orgid)
              nodeid (create-test-node orgid siteid)
              groupid (create-test-lighting-group orgid siteid {:nodeList [nodeid]})
              dhprofileid (create-test-dhprofile orgid siteid default-test-dhprofile)]

          (is (= {:result (-> default-test-dhprofile
                              (assoc :dhprofileid dhprofileid
                                     :autocalibrate true
                                     :sites []
                                     :groups [{:groupid groupid
                                               :name "Test Lighting Group"}]
                                     :nodes [{:nodeid nodeid}]))
                  :success true}
                 (run {:type "applyDHtoGroup"
                       :user user
                       :orgprops {:orgid orgid}
                       :siteprops {:siteid siteid}
                       :dhprofileprops {:dhprofileid dhprofileid}
                       :groupprops {:groupids [groupid]}})))
          (is (= {:success true}
                 (run {:type "removeDHProfileTrigger"
                       :user user
                       :orgprops {:orgid orgid}
                       :siteprops {:siteid siteid}
                       :dhprofileprops {:dhprofileid dhprofileid}
                       :nodeprops {:nodeid nodeid}})))
          (is (= {:success false
                  :error {:message "No triggers present in DHProfile"
                          :data {:dhprofileid dhprofileid
                                 :controllers [nodeid]}}}
                 (update (cape-calibrate dhprofileid)
                         :error json/read-str :key-fn keyword))))
        (finally
          (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;"))))
    (testing "Fail when all triggers do not respond in time"
      (try
        (let [orgid (create-test-org)
              siteid (create-test-site orgid)
              nodeid (create-test-node orgid siteid)
              groupid (create-test-lighting-group orgid siteid {:nodeList [nodeid]})
              dhprofileid (create-test-dhprofile orgid siteid default-test-dhprofile)]
          (is (= {:result (-> default-test-dhprofile
                              (assoc :dhprofileid dhprofileid
                                     :autocalibrate true
                                     :sites []
                                     :groups [{:groupid groupid
                                               :name "Test Lighting Group"}]
                                     :nodes [{:nodeid nodeid}]))
                  :success true}
                 (run {:type "applyDHtoGroup"
                       :user user
                       :orgprops {:orgid orgid}
                       :siteprops {:siteid siteid}
                       :groupprops {:groupids [groupid]}
                       :dhprofileprops {:dhprofileid dhprofileid}})))
          (is (= {:success false
                  :error {:message "Did not get a ambient light response from all trigger nodes."
                          :data {:dhprofileid dhprofileid
                                 :triggers [nodeid]
                                 :responses []}}}
                 (with-redefs [calibration-timeout 10]
                   (update (cape-calibrate dhprofileid)
                           :error json/read-str :key-fn keyword)))))
        (finally
          (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;"))))
    (testing "Succeed."
      (try
        (let [orgid (create-test-org)
              siteid (create-test-site orgid)
              nodeid (create-test-node orgid siteid)
              groupid (create-test-lighting-group orgid siteid {:nodeList [nodeid]})
              dhprofileid (create-test-dhprofile orgid siteid default-test-dhprofile)
              devsvccmd (atom [])]
          (is (= {:result (-> default-test-dhprofile
                              (assoc :dhprofileid dhprofileid
                                     :autocalibrate true
                                     :sites []
                                     :groups [{:groupid groupid
                                               :name "Test Lighting Group"}]
                                     :nodes [{:nodeid nodeid}]))
                  :success true}
                 (run {:type "applyDHtoGroup"
                       :user user
                       :orgprops {:orgid orgid}
                       :siteprops {:siteid siteid}
                       :dhprofileprops {:dhprofileid dhprofileid}
                       :groupprops {:groupids [groupid]}})))
          (is (= {:success true
                  :dhprofile (assoc default-test-dhprofile
                                    :dhprofileid dhprofileid
                                    :setPoint 204
                                    :sites []
                                    :groups [{:groupid groupid
                                              :name "Test Lighting Group"}]
                                    :nodes [{:nodeid nodeid}])}
                 (with-redefs [dev-ctrl/query-exec-msgpk (fn [& args]
                                                           (swap! devsvccmd conj args)
                                                           (record-ambient-light nodeid 200000))]
                   (cape-calibrate dhprofileid))))
          (is (= [[{:nodeprops {:type "LightingForceState"
                                :nodeid [nodeid]
                                :level (:maxDrive default-test-dhprofile)
                                :timeout 0
                                :harvesting true}}]]
                 @devsvccmd)))
        (finally
          (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;"))))
    (testing "Fail when there is already an active calibration"
      (try
        (let [orgid (create-test-org)
              siteid (create-test-site orgid)
              nodeid (create-test-node orgid siteid)
              groupid (create-test-lighting-group orgid siteid {:nodeList [nodeid]})
              dhprofileid (create-test-dhprofile orgid siteid default-test-dhprofile)]
          (is (= {:result (-> default-test-dhprofile
                              (assoc :dhprofileid dhprofileid
                                     :autocalibrate true
                                     :sites []
                                     :groups [{:groupid groupid
                                               :name "Test Lighting Group"}]
                                     :nodes [{:nodeid nodeid}]))
                  :success true}
                 (run {:type "applyDHtoGroup"
                       :user user
                       :orgprops {:orgid orgid}
                       :siteprops {:siteid siteid}
                       :dhprofileprops {:dhprofileid dhprofileid}
                       :groupprops {:groupids [groupid]}})))
          (let [calibration (future
                              (calibrate dhprofileid))]
            ;; Busy-wait for above calibration to start.
            (while (not (@@#'dh/suspended-profiles
                         dhprofileid)))
            (is (= {:success false
                    :error {:message "Calibration already in progress",
                            :data {:dhprofileid dhprofileid}}}
                   (update (cape-calibrate dhprofileid)
                           :error json/read-str :key-fn keyword)))))
        (finally
          (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;"))))))

(deftest autocalibrate-at-1am
  (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;")
  (let [orgid (create-test-org)
        [site-no-profiles
         site-one-profile
         site-three-lgs
         site-autocalibrate-opt-out] (repeatedly #(create-test-site orgid))

        only-dhprofile (create-test-dhprofile orgid site-one-profile)
        node (create-test-node orgid site-one-profile)
        lg (create-test-lighting-group orgid
                                       site-one-profile
                                       {:nodeList [node]})


        [dhprofileid1
         dhprofileid2] (repeatedly #(create-test-dhprofile orgid site-three-lgs))
        [nodeid1
         nodeid2
         nodeid3] (repeatedly #(create-test-node orgid site-three-lgs))
        [lg1
         lg2
         lg3] (map #(create-test-lighting-group orgid
                                                site-three-lgs
                                                {:nodeList [%]})
                   [nodeid1
                    nodeid2
                    nodeid3])

        autocalibrate-profile (create-test-dhprofile orgid site-autocalibrate-opt-out)
        no-autocalibrate-profile (create-test-dhprofile orgid
                                                        site-autocalibrate-opt-out
                                                        {:autocalibrateoptout true})
        [node-autocalibrate
         node-no-autocalibrate] (repeatedly #(create-test-node orgid site-autocalibrate-opt-out))
        [lg-autocalibrate
         lg-no-autocalibrate] (map #(create-test-lighting-group orgid
                                                                site-autocalibrate-opt-out
                                                                {:nodeList [%]})
                                   [node-autocalibrate
                                    node-no-autocalibrate])

        timezone "America/Los_Angeles"
        tz (time-core/time-zone-for-id timezone)
        just-past-one (DateTime. 2016 12 14
                                 1 7 0
                                 tz)]

    (testing "Setting up DHProfile/LG associations"
      (is (= {:result (-> default-test-dhprofile
                          (assoc :autocalibrate true
                                 :dhprofileid only-dhprofile
                                 :sites []
                                 :groups [{:groupid lg
                                           :name "Test Lighting Group"}]
                                 :nodes [{:nodeid node}]))
              :success true}
             (run {:type "applyDHtoGroup"
                   :user user
                   :orgprops {:orgid orgid}
                   :siteprops {:siteid site-one-profile}
                   :groupprops {:groupids [lg]}
                   :dhprofileprops {:dhprofileid only-dhprofile}})))
      (is (= {:result (-> default-test-dhprofile
                          (assoc :autocalibrate true
                                 :dhprofileid dhprofileid1
                                 :sites []
                                 :groups [{:groupid lg1
                                           :name "Test Lighting Group"}
                                          {:groupid lg2
                                           :name "Test Lighting Group"}
                                          {:groupid lg3
                                           :name "Test Lighting Group"}]
                                 :nodes [{:nodeid nodeid1}
                                         {:nodeid nodeid2}
                                         {:nodeid nodeid3}])
                          vectors-to-sets)
              :success true}
             (-> {:type "applyDHtoGroup"
                  :user user
                  :orgprops {:orgid orgid}
                  :siteprops {:siteid site-three-lgs}
                  :groupprops {:groupids [lg1 lg2 lg3]}
                  :dhprofileprops {:dhprofileid dhprofileid1}}
                 run
                 vectors-to-sets)))
      (is (= {:result (-> default-test-dhprofile
                          (assoc :autocalibrate true
                                 :dhprofileid autocalibrate-profile
                                 :sites []
                                 :groups [{:groupid lg-autocalibrate
                                           :name "Test Lighting Group"}]
                                 :nodes [{:nodeid node-autocalibrate}]))
              :success true}
             (run {:type "applyDHtoGroup"
                   :user user
                   :orgprops {:orgid orgid}
                   :siteprops {:siteid site-autocalibrate-opt-out}
                   :groupprops {:groupids [lg-autocalibrate]}
                   :dhprofileprops {:dhprofileid autocalibrate-profile}})))
      (is (= {:result (-> default-test-dhprofile
                          (assoc :autocalibrate true
                                 :autocalibrateoptout true
                                 :dhprofileid no-autocalibrate-profile
                                 :sites []
                                 :groups [{:groupid lg-no-autocalibrate
                                           :name "Test Lighting Group"}]
                                 :nodes [{:nodeid node-no-autocalibrate}]))
              :success true}
             (run {:type "applyDHtoGroup"
                   :user user
                   :orgprops {:orgid orgid}
                   :siteprops {:siteid site-autocalibrate-opt-out}
                   :groupprops {:groupids [lg-no-autocalibrate]}
                   :dhprofileprops {:dhprofileid no-autocalibrate-profile}}))))


    (testing "Confirm our datetime is in the expected timezone"
      (is (true? (contains? (set (recently-1am just-past-one))
                            timezone))))
    (testing "Confirm our site is in the expected timezone"
      (is (= {site-no-profiles timezone
              site-one-profile timezone
              site-three-lgs timezone
              site-autocalibrate-opt-out timezone}
             (->> {:type "getAllSitesForOrg"
                   :user user
                   :orgprops {:orgid orgid}}
                  run
                  :items
                  (map (juxt :siteid :time_zone))
                  (into {})))))
    (testing "Confirm our profiles are in the expected timezone"
      (is (= #{only-dhprofile
               dhprofileid1
               autocalibrate-profile}
             (->> just-past-one
                  dhprofiles-to-calibrate
                  (map :dhprofileid)
                  set))))
    (testing "Confirm profiles calibrate"
      (let [devsvccmd (atom [])]
        (with-redefs [dev-ctrl/query-exec-msgpk (fn [& args]
                                                  (swap! devsvccmd conj args)
                                                  (mapv #(record-ambient-light % 200000)
                                                        [node
                                                         nodeid1 nodeid2 nodeid3
                                                         node-autocalibrate]))]
          (is (= 1
                 (autocalibration just-past-one)))
          (is (= (vectors-to-sets [(list {:nodeprops {:type "LightingForceState"
                                                      :nodeid [node]
                                                      :level 83
                                                      :timeout 0
                                                      :harvesting true}})
                                   (list {:nodeprops {:type "LightingForceState"
                                                      :nodeid [nodeid1
                                                               nodeid2
                                                               nodeid3]
                                                      :level 83
                                                      :timeout 0
                                                      :harvesting true}})
                                   (list {:nodeprops {:type "LightingForceState"
                                                      :nodeid [node-autocalibrate]
                                                      :level 83
                                                      :timeout 0
                                                      :harvesting true}})])
                 (vectors-to-sets @devsvccmd))))))
    (testing "Confirm no profiles remain to be calibrated"
      (is (= #{}
             (->> just-past-one
                  dhprofiles-to-calibrate
                  (map :dhprofileid)
                  set))))
    (let [new-node (create-test-node orgid site-one-profile)]
      (testing "Confirm adding a node to a LG with a DH Profile qualifies profile for recalibration"
        (is (= {:result {:siteid site-one-profile
                         :groupid lg
                         :nodeids [new-node]}
                :success true}
               (run {:type "addNodeToGroup"
                     :user user
                     :orgprops  {:orgid orgid}
                     :siteprops {:siteid site-one-profile}
                     :groupprops {:groupid lg}
                     :nodeprops {:nodeid new-node}})))
        (is (= (-> {:dhprofile (assoc default-test-dhprofile
                                      :dhprofileid only-dhprofile
                                      :setPoint 204
                                      :autocalibrate true
                                      :sites []
                                      :groups [{:groupid lg
                                                :name "Test Lighting Group"}]
                                      :nodes [{:nodeid node}
                                              {:nodeid new-node}])
                    :success true}
                   vectors-to-sets)
               (-> {:type "getDHProfile"
                    :user user
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid site-one-profile}
                    :dhprofileprops {:dhprofileid only-dhprofile}}
                   run
                   vectors-to-sets)))
        (is (= #{only-dhprofile}
               (->> just-past-one
                    dhprofiles-to-calibrate
                    (map :dhprofileid)
                    set))))
      (testing "Recalibrate with additional node"
        (with-redefs [dev-ctrl/query-exec-msgpk (fn [& _]
                                                  (mapv #(record-ambient-light % 200000)
                                                        [node new-node]))]
          (is (= 1
                 (autocalibration just-past-one))))
        (is (= #{}
               (->> just-past-one
                    dhprofiles-to-calibrate
                    (map :dhprofileid)
                    set))))
      (testing "Confirm removing nodes qualifies profile for recalibration"
        (is (= {:result {:siteid site-one-profile
                         :groupid lg
                         :nodeid new-node}
                :success true}
               (run {:type "removeNodeFromGroup"
                     :user user
                     :orgprops  {:orgid orgid}
                     :siteprops {:siteid site-one-profile}
                     :groupprops {:groupid lg}
                     :nodeprops {:nodeid new-node}})))
        (is (= (-> {:dhprofile (assoc default-test-dhprofile
                                      :dhprofileid only-dhprofile
                                      :setPoint 204
                                      :autocalibrate true
                                      :sites []
                                      :groups [{:groupid lg
                                                :name "Test Lighting Group"}]
                                      :nodes [{:nodeid node}])
                    :success true}
                   vectors-to-sets)
               (-> {:type "getDHProfile"
                    :user user
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid site-one-profile}
                    :dhprofileprops {:dhprofileid only-dhprofile}}
                   run
                   vectors-to-sets)))
        (is (= #{only-dhprofile}
               (->> just-past-one
                    dhprofiles-to-calibrate
                    (map :dhprofileid)
                    set)))
        (testing "Recalibrate with one less node"
          (with-redefs [dev-ctrl/query-exec-msgpk (fn [& _]
                                                    (record-ambient-light node 200000))]
            (is (= 1
                   (autocalibration just-past-one))))
          (is (= #{}
                 (->> just-past-one
                      dhprofiles-to-calibrate
                      (map :dhprofileid)
                      set))))))
    (testing "Confirm removing LG qualifies profile for recalibration"
      (is (= (-> {:dhprofile (assoc default-test-dhprofile
                                    :dhprofileid dhprofileid1
                                    :setPoint 204
                                    :sites []
                                    :groups [{:groupid lg1
                                              :name "Test Lighting Group"}
                                             {:groupid lg2
                                              :name "Test Lighting Group"}
                                             {:groupid lg3
                                              :name "Test Lighting Group"}]
                                    :nodes [{:nodeid nodeid1}
                                            {:nodeid nodeid2}
                                            {:nodeid nodeid3}])
                  :success true}
                 vectors-to-sets)
             (-> {:type "getDHProfile"
                  :user user
                  :orgprops {:orgid orgid}
                  :siteprops {:siteid site-three-lgs}
                  :dhprofileprops {:dhprofileid dhprofileid1}}
                 run
                 vectors-to-sets)))
      (is (= {:group {:groupid lg2
                      :name "Test Lighting Group"
                      :description ""
                      :type "lighting"
                      :nodeList [nodeid2]
                      :nodes [{:model "unode-v4", :nodeid nodeid2}]
                      :schedules []
                      :pdprofiles []
                      :etdhprofiles []
                      :dhprofiles []}
              :success true}
             (run {:type "updateGroup"
                   :user user
                   :orgprops {:orgid orgid}
                   :siteprops {:siteid site-three-lgs}
                   :groupprops (-> {:type "getGroup"
                                    :user user
                                    :orgprops {:orgid orgid}
                                    :siteprops {:siteid site-three-lgs}
                                    :groupprops {:groupid lg2}}
                                   run
                                   :group
                                   (assoc :dhprofiles []))})))
      (is (= (-> {:dhprofile (assoc default-test-dhprofile
                                    :dhprofileid dhprofileid1
                                    :autocalibrate true
                                    :setPoint 204
                                    :sites []
                                    :groups [{:groupid lg1
                                              :name "Test Lighting Group"}
                                             {:groupid lg3
                                              :name "Test Lighting Group"}]
                                    :nodes [{:nodeid nodeid1}
                                            {:nodeid nodeid3}])
                  :success true}
                 vectors-to-sets)
             (-> {:type "getDHProfile"
                  :user user
                  :orgprops {:orgid orgid}
                  :siteprops {:siteid site-three-lgs}
                  :dhprofileprops {:dhprofileid dhprofileid1}}
                 run
                 vectors-to-sets)))
      (is (= #{dhprofileid1}
             (->> just-past-one
                  dhprofiles-to-calibrate
                  (map :dhprofileid)
                  set)))
      (testing "Recalibrate with one less lighting group"
        (with-redefs [dev-ctrl/query-exec-msgpk (fn [& _]
                                                  (mapv #(record-ambient-light % 200000)
                                                        [nodeid1 nodeid3]))]
          (is (= 1
                 (autocalibration just-past-one))))))
    (testing "Confirm moving LG from one DH Profile to another qualifies *both* for recalibration"
      (is (= {:group {:groupid lg3
                      :name "Test Lighting Group"
                      :description ""
                      :type "lighting"
                      :nodeList [nodeid3]
                      :nodes [{:model "unode-v4", :nodeid nodeid3}]
                      :schedules []
                      :pdprofiles []
                      :etdhprofiles []
                      :dhprofiles [{:dhprofileid dhprofileid2}]}
              :success true}
             (run {:type "updateGroup"
                   :user user
                   :orgprops {:orgid orgid}
                   :siteprops {:siteid site-three-lgs}
                   :groupprops (-> {:type "getGroup"
                                    :user user
                                    :orgprops {:orgid orgid}
                                    :siteprops {:siteid site-three-lgs}
                                    :groupprops {:groupid lg3}}
                                   run
                                   :group
                                   (assoc :dhprofiles [{:dhprofileid dhprofileid2}]))})))
      (is (= #{dhprofileid1
               dhprofileid2}
             (->> just-past-one
                  dhprofiles-to-calibrate
                  (map :dhprofileid)
                  set))))))

(deftest read-only-setpoint
  (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;")
  (let [orgid (create-test-org)
        siteid (create-test-site orgid)
        nodeid (create-test-node orgid siteid)
        groupid (create-test-lighting-group orgid siteid {:nodeList [nodeid]})
        dhprofile default-test-dhprofile
        dhprofileid (create-test-dhprofile orgid siteid default-test-dhprofile)]
    (testing "Initial DH Profile `setPoint` is ignored"
      (is (= {:dhprofile (-> dhprofile
                             (assoc :dhprofileid dhprofileid
                                    :autocalibrate true
                                    :sites []
                                    :groups []
                                    :nodes []))
              :success true}
             (run {:type "getDHProfile"
                   :user user
                   :orgprops {:orgid orgid}
                   :siteprops {:siteid siteid}
                   :dhprofileprops {:dhprofileid dhprofileid}}))))
    (testing "Updating DH Profile `setPoint` is ignored"
      (is (= {:dhprofile (-> dhprofile
                             (assoc :dhprofileid dhprofileid
                                    :autocalibrate true
                                    :gain 0.75
                                    :sites []
                                    :groups []
                                    :nodes []))
              :site (select-keys default-test-site [:latitude
                                                    :longitude])
              :success true}
             (run {:type "updateDHProfile"
                   :user user
                   :orgprops {:orgid orgid}
                   :siteprops {:siteid siteid}
                   :dhprofileprops (assoc dhprofile
                                          :dhprofileid dhprofileid
                                          :gain 0.75
                                          :setPoint 70
                                          :sites []
                                          :groups []
                                          :nodes [])})))
      (is (= {:dhprofile (-> dhprofile
                             (assoc :dhprofileid dhprofileid
                                    :autocalibrate true
                                    :gain 0.75
                                    :sites []
                                    :groups []
                                    :nodes []))
              :success true}
             (run {:type "getDHProfile"
                   :user user
                   :orgprops {:orgid orgid}
                   :siteprops {:siteid siteid}
                   :dhprofileprops {:dhprofileid dhprofileid}}))))
    (testing "Assigning DH Profile"
      (is (= {:result (-> dhprofile
                          (assoc :dhprofileid dhprofileid
                                 :autocalibrate true
                                 :gain 0.75
                                 :sites []
                                 :groups [{:groupid groupid
                                           :name "Test Lighting Group"}]
                                 :nodes [{:nodeid nodeid}]))
              :success true}
             (run {:type "applyDHtoGroup"
                   :user user
                   :orgprops {:orgid orgid}
                   :siteprops {:siteid siteid}
                   :dhprofileprops {:dhprofileid dhprofileid}
                   :groupprops {:groupids [groupid]}}))))
    (testing "Calibrating DH Profile"
      (is (= {:success true
              :dhprofile (assoc default-test-dhprofile
                                :dhprofileid dhprofileid
                                :gain 0.75
                                :setPoint 204
                                :sites []
                                :groups [{:groupid groupid
                                          :name "Test Lighting Group"}]
                                :nodes [{:nodeid nodeid}])}
             (with-redefs [dev-ctrl/query-exec-msgpk (fn [& _]
                                                       (record-ambient-light nodeid 200000))]
               (run {:type "calibrateDHProfile"
                     :user user
                     :siteprops {:siteid siteid}
                     :orgprops {:orgid orgid}
                     :dhprofileprops {:dhprofileid dhprofileid}})))))
    (testing "Updating calibrated DHProfile ignores `setPoint`"
      (is (= {:dhprofile (assoc dhprofile
                                :dhprofileid dhprofileid
                                :setPoint 204
                                :gain 0.5
                                :sites []
                                :groups [{:groupid groupid
                                          :name "Test Lighting Group"}]
                                :nodes [{:nodeid nodeid}])
              :site (select-keys default-test-site [:latitude
                                                    :longitude])
              :success true}
             (run {:type "updateDHProfile"
                   :user user
                   :orgprops {:orgid orgid}
                   :siteprops {:siteid siteid}
                   :dhprofileprops (assoc dhprofile
                                          :dhprofileid dhprofileid
                                          :gain 0.5
                                          :setPoint 17)})))
      (is (= {:dhprofile (assoc dhprofile
                                :dhprofileid dhprofileid
                                :setPoint 204
                                :gain 0.5
                                :sites []
                                :groups [{:groupid groupid
                                          :name "Test Lighting Group"}]
                                :nodes [{:nodeid nodeid}])
              :success true}
             (run {:type "getDHProfile"
                   :user user
                   :orgprops {:orgid orgid}
                   :siteprops {:siteid siteid}
                   :dhprofileprops {:dhprofileid dhprofileid}}))))))
