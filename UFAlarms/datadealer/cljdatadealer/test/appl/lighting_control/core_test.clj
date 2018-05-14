(ns appl.lighting-control.core-test
  (:require [appl.lighting-control.core :refer :all]
            [appl.proximity-dimming-test :as pd-test]
            [clojure.test :refer :all]
            [dealer
             [devsvcctrl :as dev-ctrl]
             [devsvcctrl-test :as dev-ctrl-test]]
            [mqtt.fixture :as mqtt]
            [kafka.mock :as kafka]
            [utils
             [cape-test :refer [vectors-to-sets
                                run
                                *user*
                                uuid
                                create-test-org
                                create-test-site
                                default-test-site
                                create-test-node
                                create-test-lighting-group
                                create-test-pdprofile
                                default-test-pdprofile]]
             [cassandra_fixture :as cass]
             [neo4j-fixture :as neo4j-fixture]]))

(use-fixtures :once (join-fixtures [cass/cassandra-fixture
                                    neo4j-fixture/neo4j-fixture
                                    kafka/kafka-fixture
                                    mqtt/mqtt-fixture
                                    dev-ctrl-test/device-service-control-fixture
                                    utils.cape-test/mock-google-api]))

(defn send-lfs
  [{:keys [nodes
           level]}]
  (dev-ctrl/query-exec-msgpk {:nodeprops {:type "LightingForceState"
                                          :nodeid nodes
                                          :level level
                                          :timeout 0}}))

(deftest lfa-after-profile-removal
  (let [orgid (create-test-org)
        siteid (create-test-site orgid)
        [node-a
         node-b
         :as nodeList] (repeatedly 2 #(create-test-node orgid siteid))
        lg (create-test-lighting-group orgid
                                       siteid
                                       {:nodeList nodeList})
        {:keys [minLevel
                maxLevel]} default-test-pdprofile

        pdprofileid (create-test-pdprofile orgid siteid)

        apply-pdprofile {:type "applyPDtoGroup"
                         :user *user*
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :pdprofileprops {:pdprofileid pdprofileid}
                         :groupprops {:groupids [lg]}}

        dev-ctrl-output (atom [])

        lfs {"name" "LightingForceState"
             "pri" 4
             "mask" 1
             "qualifiers" "undefined"
             "ftype" "Volatile"}
        expected-dev-ctrl [;; Confirm PD is working
                           [(assoc lfs
                                   "level" maxLevel
                                   "nodeid" #{node-a
                                              node-b})]
                           ;; Verify LFS
                           [(assoc lfs
                                   "pri" 3
                                   "level" 12
                                   "nodeid" #{node-b})]
                           ;; Verify LSA
                           [{"name" "LightingSetAuto"
                             "nodeid" #{node-a}}]]]

    (is (= (-> {:success true
                :result (assoc default-test-pdprofile
                               :pdprofileid pdprofileid
                               :sites []
                               :groups [{:groupid lg
                                         :name "Test Lighting Group"}]
                               :nodes (mapv #(do {:nodeid %})
                                            nodeList))}
               vectors-to-sets)
           (-> apply-pdprofile
               run
               vectors-to-sets)))

    (with-redefs [dev-ctrl/sendtodevsvc (fn [& args]
                                          (swap! dev-ctrl-output
                                                 conj
                                                 (update-in (vec args)
                                                            [0 "nodeid"]
                                                            set)))]
      ;; Trigger PD by registering motion on `node-a`
      (pd-test/wave node-a)

      ;; Send a priority 3 LFS to `node-b`
      (send-lfs {:nodes [node-b]
                 :level 12})

      ;; Update the Lighting Group to remove the PDProfile.
      (let [groupprops (-> {:type "getGroup"
                            :user *user*
                            :orgprops {:orgid orgid}
                            :siteprops {:siteid siteid}
                            :groupprops {:groupid lg}}
                           run
                           :group
                           (assoc :pdprofiles [])
                           (update :nodeList set))]
       (is (= {:groupid lg
               :name "Test Lighting Group"
               :description ""
               :nodeList (set nodeList)
               :type "lighting"
               :schedules []
               :etdhprofiles []
               :dhprofiles []
               :pdprofiles []}
              (dissoc groupprops :nodes)))
       (is (= (-> {:success true
                   :group groupprops}
                  vectors-to-sets)
              (-> {:type "updateGroup"
                   :user *user*
                   :orgprops {:orgid orgid}
                   :siteprops {:siteid siteid}
                   :groupprops groupprops}
                  run
                  vectors-to-sets)))))

    (is (= expected-dev-ctrl
           @dev-ctrl-output))))
