(ns utils.apply-config-test
  (:require [biscuit.core :as digest]
            [clojure
             [test :refer :all]
             [walk :refer :all]]
            [clojure.data.json :as json]
            [clojure.tools.logging :refer :all]
            [dealer
             [devsvcctrl :as dctrl]
             [devsvcctrl-test :as dctrl-test]]
            [mqtt.fixture :as mqtt]
            [kafka.mock :as kafka]
            [neowrap.neowrapper :as neo4j]
            [utils cape-test
             [cape :as cape]
             [cassandra_fixture :as cass]
             [config :as cfg]
             [neo4j-fixture :as neo4j-fixture]
             [nodeconfig :as nodeconfig]]))

(use-fixtures :once (join-fixtures [cass/cassandra-fixture
                                    neo4j-fixture/neo4j-fixture
                                    mqtt/mqtt-fixture
                                    kafka/kafka-fixture
                                    dctrl-test/device-service-control-fixture
                                    utils.cape-test/mock-google-api]))

(def orgid "config-testorg")
(def siteid "config-testsite")
(def group1 "config-group1")
(def group2 "config-group2")
(def configid "config-v4")

(with-test
  (defn run [req]
    (keywordize-keys (json/read-str (cape/executer-main (stringify-keys req)))))

  (defn create-nodes
    ([nodeids](create-nodes nodeids "unode-v4"))
    ([nodeids model]
    (let [node {:type "createNode"
                :user "uberuser"
                :orgprops  {:orgid orgid}
                :siteprops {:siteid siteid}
                :nodeprops {:nodeid    "node"
                            :name      "Test Node"
                            :model     model
                            :configToken "default"
                            :latitude  "37.383712"
                            :longitude "-121.989921"}}]
      (map (fn [ii]
        (let [n (run (-> node
                         (assoc-in [:nodeprops :nodeid] (str "cnode" ii))
                         (assoc-in [:nodeprops :name] (str "cnode" ii))))]
          (is (:success n))
          (:node (:nodeid n))
          (str "cnode" ii)
          ))
            nodeids))))

  (defn create-config [props]
    (let [config {:type "createConfig"
                          :user      "uberuser"
                          :orgprops  {:orgid orgid}
                          :siteprops {:siteid siteid}
                          :configprops props}]
      (is (:success (run config)))
      config))

  (defn update-config [props]
    (let [config {:type "updateConfig"
                  :user      "uberuser"
                  :orgprops  {:orgid orgid}
                  :siteprops {:siteid siteid}
                  :configprops props}]
      (is (:success (run config)))
      config))

  (defn get-expected-config
    [config]
    (let [server (:server (nodeconfig/default-node-config))
          config-translated (nodeconfig/translate-config config)
          {token :token kvpairs :kvpairs} (cfg/calculate-token (assoc config-translated :server server))
          expected (stringify-keys (merge kvpairs {:token token}))]
      expected))

  (defn update-config-server
    [server]
    (neo4j/executeQuery (format "Match (c:Config {configid: \"default\"}) SET c.server=\"%s\"" server)))

  (defn apply-server-to-node [nodeList server]
    (let [server {:type "applyServerToNodes"
                :user      "uberuser"
                :orgprops  {:orgid orgid}
                :siteprops {:siteid siteid}
                :nodeprops {:nodeList nodeList
                            :server server}}
          result (run server)]
      (is (:success result))
      (:config result)))

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

  (defn get-configs []
    (let [config {:type "getAllConfigs"
                     :user "uberuser"
                     :orgprops  {:orgid orgid}
                     :siteprops {:siteid siteid}}
          received (run config)]
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

  (defn make-group [type groupid nodeids]
    (let [group {:type type
                 :user "uberuser"
                 :orgprops  {:orgid orgid}
                 :siteprops {:siteid siteid}
                 :groupprops {:groupid groupid
                              :description groupid
                              :nodeList nodeids
                              :name groupid
                              :type "organizational"}}
          received (run group)]
      (is (:success received))
      (:group received)))

  (defn apply-config [type configid & [groupid nodeids]]
    (let [apply {:type          type
                 :user          "uberuser"
                 :orgprops      {:orgid orgid}
                 :siteprops     {:siteid siteid}
                 :groupprops    {:groupid groupid}
                 :nodeprops     {:nodeids nodeids}
                 :configprops   {:configid configid}}
          applied (run apply)]
      (is (:success applied))))

  (defn node-resp [nodeids]
    (map #(do {:nodename % :nodeid %}) nodeids))

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
        configPropsDelete {:type "deleteConfig"
                          :user "uberuser"
                          :orgprops {:orgid orgid}
                          :siteprops {:siteid siteid}
                          :configprops {:configid configid}}
        orgCreated (run orgPropsCreate)
        siteCreated (run sitePropsCreate)
        platform "dev.sensity.com"
        configUpdated (update-config-server platform)
        nodes [{:model "unode-v4" :nodeid "N1"}
               {:model "unode-v4" :nodeid "N2"}
               {:model "unode-v5" :nodeid "N3"}
               {:model "unode-v3" :nodeid "N4"}
               {:model "unode-v2" :nodeid "N5"}]

        conf-v4-default (nodeconfig/get-full-conf-by-model "unode-v4")
        conf-v4 (merge conf-v4-default {:configid configid :name "config v4" :model "unode-v4" :server "junk.sensity.com"})
        conf-update-v4 (merge conf-v4 {:server "foo"})
        expected-conf-v4 (get-expected-config conf-v4)
        expected-update-conf-v4 (get-expected-config conf-update-v4)
        {server-cfgToken :token} (cfg/calculate-token {:server platform})
        device-command (atom nil)]

      (with-redefs [dctrl/sendtodevsvc #(reset! device-command %)]

      (create-config conf-v4)
      
      (is (= (get expected-conf-v4 "server") (:server (nodeconfig/default-node-config))))

      (update-config conf-update-v4)

      (is (= (get expected-update-conf-v4 "server") (:server (nodeconfig/default-node-config))))

      (is (:success orgCreated))
      (is (:success siteCreated))

      (def allnodeids (create-nodes (range 1 7)))
      (def nodeids (take 5 allnodeids))

      (def successful-config-redirect
        #{{:nodeid "cnode3", :server "prod.sensity.com"}
          {:nodeid "cnode4", :server "prod.sensity.com"}
          {:nodeid "cnode1", :server "prod.sensity.com"}
          {:nodeid "cnode5", :server "prod.sensity.com"}
          {:nodeid "cnode2", :server "prod.sensity.com"}})

      (def semi-successful-config-redirect
        #{{:nodeid "cnode3", :server "next.sensity.com"}
          {:nodeid "cnode4", :server "next.sensity.com"}
          {:nodeid "cnode1", :server "next.sensity.com"}
          {:nodeid "cnode5", :server "next.sensity.com"}
          {:nodeid "cnode2", :server "next.sensity.com"}})

      (def device-config-command {"name" "ConfigResp"
                                  "configtype" "configupdate"
                                  "nodeid" nodeids
                                  "kvpairs" expected-conf-v4})

      (Thread/sleep 300)

      (is (= (set (apply-server-to-node nodeids "prod.sensity.com")) successful-config-redirect))

      (Thread/sleep 300)

      (debug "Apply server to Node")
      (debug @device-command)

      (is (= (get @device-command "name") "ConfigResp"))
      (is (= (get @device-command "configtype") "serverupdate"))
      (is (= (get-in @device-command ["kvpairs" "server"]) "prod.sensity.com"))

      (reset! device-command {})
      (is (= (set (apply-server-to-node (conj nodeids "none") "next.sensity.com")) semi-successful-config-redirect))

      (Thread/sleep 300)

      (debug "Apply server to Node - Error case")
      (debug @device-command)

      (make-group "createGroup" group1 nodeids)

      (apply-config "applyConfigToGroup" configid group1)

      (Thread/sleep 300)

      (debug "Apply Config")
      (debug  @device-command)
      (is (= device-config-command @device-command))

      (apply-config "applyConfigToNodes" configid nil (seq ["cnode6"]))

      (Thread/sleep 300)

      (is (= (assoc device-config-command "nodeid" (seq ["cnode6"])) @device-command))

      (is (:error (run configPropsDelete)))
    )))
