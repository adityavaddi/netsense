(ns acl.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.tools.logging :refer :all]
            [dealer.devsvcctrl-test :as dev-ctrl-test]
            [mqtt.fixture :as mqtt]
            [kafka.mock :as kafka]
            [utils.cape-test :refer [vectors-to-sets
                                     uuid
                                     run
                                     *user*
                                     create-test-org
                                     default-test-site
                                     create-test-site
                                     create-test-node
                                     create-test-lighting-group
                                     default-test-etdhprofile
                                     create-test-etdhprofile]]
            [utils.cassandra_fixture :as cass]
            [utils.neo4j-fixture :as neo4j-fixture]))

(use-fixtures :once (join-fixtures [cass/cassandra-fixture
                                    neo4j-fixture/neo4j-fixture
                                    kafka/kafka-fixture
                                    mqtt/mqtt-fixture
                                    dev-ctrl-test/device-service-control-fixture
                                    utils.cape-test/mock-google-api]))


(deftest etdh-trigger-acl-test
  (let [orgid (create-test-org)
        siteid (create-test-site orgid)
        nodeid-1 (create-test-node orgid
                                   siteid)
        lg (create-test-lighting-group orgid
                                       siteid
                                       {:nodeList [nodeid-1]})
        etdhprofileid (create-test-etdhprofile orgid
                                               siteid)
        
        apply-etdhprofile {:type "applyETDHtoGroup"
                           :user "uberuser"
                           :orgprops {:orgid orgid}
                           :siteprops {:siteid siteid}
                           :etdhprofileprops {:etdhprofileid etdhprofileid}
                           :groupprops {:groupids [lg]}}]

    (let [{:keys [success]} (run apply-etdhprofile)]
      (is (= true
             success)))

    (let [{:keys [success
                  items]} (run (merge {:type "getAllETDHProfileTriggers"
                                       :user *user*} apply-etdhprofile))]
      (is (= true
             success))
      )

    (let [{:keys [success
                  items]} (run (merge {:type "addETDHProfileTriggers"
                                       :user *user*} apply-etdhprofile))]
      (is (= true
             success)))

    (let [{:keys [success
                  items]} (run (merge {:type "removeETDHProfileTriggers"
                                       :nodeprops {:nodeids [nodeid-1]}
                                       :user *user*} apply-etdhprofile))]
      (is (= true
             success)))

      (let [userPropsCreated {:type     "createUser"
                              :user      "uberuser"
                          :orgprops  {:orgid orgid}
                          :userprops {:userid   "testuser@sensity.com"
                                      :email    "testuser@sensity.com"
                                      :name     "Test User"
                                      :roles    "parking_manager"
                                      :sites    "testsite"
                                      :title    "User in Test"
                                      :phone    "8675309"}}

            userCreated (run userPropsCreated)]

        (is (true? (:success userCreated)))

                
        (let [cape-return (try 
                            (run (merge apply-etdhprofile
                                        {:type "addETDHProfileTriggers"
                                         :nodeprops {:nodeids [nodeid-1]}
                                         :user "testuser@sensity.com"}))
                            (catch Throwable t
                              t))]

          (is (= true
                 (and (str/starts-with? (.getMessage cape-return) "Etdhprofile with id=")
                      (str/ends-with? (.getMessage cape-return) "not found"))))
          

          )



        
        (let [cape-return (try 
                                (run (merge apply-etdhprofile
                                            {:type "removeETDHProfileTriggers"
                                             :nodeprops {:nodeids [nodeid-1]}
                                             :user "testuser@sensity.com"}))
                                (catch Throwable t
                                  t))]

          (is (= true
                 (and (str/starts-with? (.getMessage cape-return) "Etdhprofile with id=")
                      (str/ends-with? (.getMessage cape-return) "not found"))))
          

        )


    )))
