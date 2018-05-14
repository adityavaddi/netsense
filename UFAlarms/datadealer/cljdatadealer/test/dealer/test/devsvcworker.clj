(ns dealer.test.devsvcworker
  (:require [clojure
             [set :refer :all]
             [test :refer :all]
             [walk :refer :all]]
            [clojure.data.json :as json]
            [clj-time
             [coerce :as time-coerce]
             [core :as time-core]]
            [dealer
             [devsvcctrl :as dev-ctrl]
             [devsvcctrl-test :as dev-ctrl-test]
             [devsvcworker :as worker]
             [mqttlistener :as listener]]
            [logging.activitylogger :as logger]
            [clojure.tools.logging :refer :all]
            [metrics.histograms :as histograms]
            [mqtt
             [core :as mqttcore]
             [core_lib :as mqttlib]
             [fixture :as mqtt]]
            [msgpack.core :as msgpk]
            [neowrap.neowrapper :as neo4j]
            [utils.async.async_chans :as ac]
            [kafka.producer :as producer]
            [utils
             [cape :as cape]
             [cassandra_intf :as cass-util]
             [cape-test :as cape-test
              :refer [create-test-site
                      create-test-org
                      create-test-node-empty
                      run
                      *user*]]
             [cassandra_fixture :as cass]
             [neo4j-fixture :as neo4j-fixture]]))

(use-fixtures :once (join-fixtures [cass/cassandra-fixture
                                    neo4j-fixture/neo4j-fixture
                                    dev-ctrl-test/device-service-control-fixture
                                    mqtt/mqtt-fixture]))


(deftest test-login-req
  (let [missingnode (msgpk/pack (stringify-keys {:name "LoginReq"
                                                 :nodeid "NS1"
                                                 :clientType "unode-v4"
                                                 :localIP "192.168.1.10"
                                                 :swVerId "deadbeef"
                                                 :configToken "e1980ac9"
                                                 :netName "ubernetwork"}))
        ubernode  (msgpk/pack (stringify-keys {:name "LoginReq"
                                               :nodeid "ubernode"
                                               :clientType "unode-v4"
                                               :localIP "192.168.1.10"
                                               :swVerId "deadbeef"
                                               :configToken "e1980ac9"
                                               :netName "ubernetwork"}))
        missingnodeconnect  (msgpk/pack (stringify-keys {:name "ConnectionStatus"
                                                         :nodeid "NS1"
                                                         :status "connected"}))
        lightsample (msgpk/pack (stringify-keys {:name "SensorSample"
                                                 :nodeid "NS1"
                                                 :sensor "lt"
                                                 :value 1.0
                                                 :time 1}))
        msgpk (atom [])
        kafkamsg (atom [])]

    ;; Send LFS to ubernode and verify it's in Cassandra
    (with-redefs [dev-ctrl/sendtodevsvc identity]
      (dev-ctrl/query-exec-msgpk {:nodeprops {:type "LightingForceState"
                                              :nodeid ["ubernode"]
                                              :level 79
                                              :timeout 10000
                                              :pri 6}})
      (let [{:keys [startdt]
             :as lastlm} (logger/get_latest_light_mode "ubernode")]
        (is (= lastlm
               {:nodeid "ubernode"
                :isscheduled false
                :harvest_trigger false
                :policy "override"
                :priority 6
                :driver 79
                :startdt startdt}))))


    (with-redefs [dev-ctrl/sendtodevsvc #(swap! msgpk conj %)
                  producer/kafka-alert #(swap! kafkamsg conj %1)]
      (worker/process-device-msg missingnode)
      (testing "A \"missing node\" should not have a `LightingForceState` sent"
        (is (= []
               (->> @msgpk
                    keywordize-keys
                    (filter (comp #{"LightingForceState"}
                                  :name))))))
      (worker/process-device-msg ubernode)
      (testing "Verify a `LightingForceState` is sent due to the earlier sent LFS."
        (is (= [{:ftype "Volatile",
                 :level 79,
                 :mask 1,
                 :name "LightingForceState",
                 :nodeid ["ubernode"],
                 :pri 6,
                 :qualifiers "undefined"}]
               (->> @msgpk
                    keywordize-keys
                    (filter (comp #{"LightingForceState"}
                                  :name))))))
      (worker/process-device-msg missingnodeconnect)
      (worker/process-device-msg lightsample))

    ;; Confirm each node (array length 4) received a schedule. Each
    ;; schedule has four slots (array contents: 4) with two slots for photocell high and low with and without network
    ;(spy :error @msgpk)
    (is (= [4 4]
           (->> @msgpk
                keywordize-keys
                (filter (comp #{"LightingScheduledEvent"}
                              :name))
                (map (comp count
                           :schedules)))))
    ;(Thread/sleep 200)

    ;; Get the Cassandra row in node_status before
    ;; Changing the assertion as DD is no more updating the node_status table on receiving lt sensor values
    (is (= (logger/get_latest_lf_nodes_statuses)
           [{:nodeid "NS1" :lig_stat nil :net_stat true :sen_stat nil :siteid "Unknown" :orgid "Unknown"}]))
    (let [assignNode {:type "assignNode"
                      :user "uberuser"
                      :orgprops  {:orgid "uberorg"}
                      :siteprops {:siteid "ubersite"}
                      :nodeprops {:nodeid "NS1" :model "unode-v4"}}
          result (-> assignNode
                     (cape/checkprops)
                     (cape/template-selector)
                     (cape/db-selector)
                     (json/read-str :key-fn keyword))]
      (is (true? (:success result)))
      (is (true? (= [{:orgid "uberorg" :siteid "ubersite"}] (logger/get_node_hierarchy "NS1"))))
      (is (= "NS1" (get-in result [:node :nodeid]))))
    ;(Thread/sleep 200)

    ;; Confirm the Cassandra row in node_status after assignNode
    ;; Changing the assertion as DD is no more updating the node_status table on receiving lt sensor values
    (is (= (logger/get_latest_site_nodes_statuses "ubersite")
          [{:nodeid "NS1" :lig_stat nil :net_stat true :sen_stat nil :siteid "ubersite" :orgid "uberorg"}]))))

(deftest test-parking-config-removed
  (let [cfg1 {:roi    {:n     "Parking_Area_Z",
                       :img   [{:v [0.0,0.0,0.0,0.0],:u [0.0,0.0,0.0,0.0]}],
                       :uuid  "Ei[({v+jvmW$hmCp?WfB",
                       :vs    [5.0, 1.7999999523162842, 1.5],
                       :spots [{:img   {:v [0.0,0.0,0.0,0.0],:u [0.0,0.0,0.0,0.0]},
                                :e     true,
                                :uuid  "!Y.emJ-nxUYKi8fXnEAT",
                                :world {:lon [0, 0, 0, 0], :lat [0,0,0,0]}},
                               {:img   {:v [0.0,0.0,0.0,0.0], :u [0.0,0.0,0.0,0.0]},
                                :e     true,
                                :uuid  "![vQW7be2fS\\/AJ\\/vM}Rg",
                                :world {:lon [0, 0, 0, 0], :lat [0,0,0,0]}},
                               {:img   {:v [0.0,0.0,0.0,0.0], :u [0.0,0.0,0.0,0.0]},
                                :e     true,
                                :uuid  "1$RJ5Y+i:MIeJY5(\\/^B0",
                                :world {:lon [0, 0, 0, 0], :lat [0,0,0,0]}},
                               {:img   {:v [0.0,0.0,0.0,0.0], :u [0.0,0.0,0.0,0.0]},
                                :e     true,
                                :uuid  "8GeWQt4yjuY!n.eJZ>TJ",
                                :world {:lon [0, 0, 0, 0], :lat [0,0,0,0]}},
                               {:img   {:v [0.0,0.0,0.0,0.0], :u [0.0,0.0,0.0,0.0]},
                                :e     true,
                                :uuid  "C5f&cZunkyNv>8oA]2>*",
                                :world {:lon [0, 0, 0, 0], :lat [0,0,0,0]}},
                               {:img   {:v [0.0,0.0,0.0,0.0], :u [0.0,0.0,0.0,0.0]},
                                :e     true,
                                :uuid  "n[{U@C7rg}FCuxdFFs{K",
                                :world {:lon [0, 0, 0, 0], :lat [0,0,0,0]}}
                               ],
                       :world [{:lon [0, 0, 0, 0],
                                :lat [0, 0, 0, 0]}]},
              :h      {:n    "Parking Area 1",
                       :e    true,
                       :des  "",
                       :ch   0,
                       :uuid "3KB:P(&oP5G@!P=on@JH",
                       :tag  "",
                       :t    1496964335136000,
                       :u    "nr"}
              :nodeid "ubernode",
              :name   "DemarcatedParkingConfig"}
        cfg2 {:roi    {:n     "Parking_Area_Z",
                       :img   [{:v [0.0,0.0,0.0,0.0],:u [0.0,0.0,0.0,0.0]}],
                       :uuid  "Ei[({v+jvmW$hmCp?WfB",
                       :vs    [5.0, 1.7999999523162842, 1.5],
                       :spots [{:img   {:v [0.0,0.0,0.0,0.0],:u [0.0,0.0,0.0,0.0]},
                                :e     true,
                                :uuid  "!Y.emJ-nxUYKi8fXnEAT",
                                :world {:lon [0, 0, 0, 0], :lat [0,0,0,0]}},
                               {:img   {:v [0.0,0.0,0.0,0.0], :u [0.0,0.0,0.0,0.0]},
                                :e     true,
                                :uuid  "![vQW7be2fS\\/AJ\\/vM}Rg",
                                :world {:lon [0, 0, 0, 0], :lat [0,0,0,0]}},
                               {:img   {:v [0.0,0.0,0.0,0.0], :u [0.0,0.0,0.0,0.0]},
                                :e     true,
                                :uuid  "8GeWQt4yjuY!n.eJZ>TJ",
                                :world {:lon [0, 0, 0, 0], :lat [0,0,0,0]}},
                               {:img   {:v [0.0,0.0,0.0,0.0], :u [0.0,0.0,0.0,0.0]},
                                :e     true,
                                :uuid  "n[{U@C7rg}FCuxdFFs{K",
                                :world {:lon [0, 0, 0, 0], :lat [0,0,0,0]}}
                               ],
                       :world [{:lon [0, 0, 0, 0],
                                :lat [0, 0, 0, 0]}]},
              :h      {:n    "Parking Area 1",
                       :e    true,
                       :des  "",
                       :ch   0,
                       :uuid "3KB:P(&oP5G@!P=on@JH",
                       :tag  "",
                       :t    1496964335136000,
                       :u    "nr"}
              ;:removed [(worker/convertUUID "1$RJ5Y+i:MIeJY5(\\/^B0") (worker/convertUUID "C5f&cZunkyNv>8oA]2>*")]
              :nodeid "ubernode",
              :name   "DemarcatedParkingConfig"}
        cfg3 {:roi    {:n     "Parking_Area_Z",
                       :img   [{:v [0.0,0.0,0.0,0.0],:u [0.0,0.0,0.0,0.0]}],
                       :uuid  "Ei[({v+jvmW$hmCp?WfB",
                       :vs    [5.0, 1.7999999523162842, 1.5],
                       :spots [],
                       :world [{:lon [0, 0, 0, 0],
                                :lat [0, 0, 0, 0]}]},
              :h      {:n    "Parking Area 1",
                       :e    true,
                       :des  "",
                       :ch   0,
                       :uuid "3KB:P(&oP5G@!P=on@JH",
                       :tag  "",
                       :t    1496964335136000,
                       :u    "nr"}
              :nodeid "ubernode",
              :name   "DemarcatedParkingConfig"}
        msg1   (msgpk/pack (stringify-keys cfg1))
        msg2   (msgpk/pack (stringify-keys cfg2))
        msg3   (msgpk/pack (stringify-keys cfg3))]
    (worker/process-device-msg msg1)
    (Thread/sleep 200)
    (let [spots (logger/get_latest_parking_info "ubersite")
          spots2 (cass-util/get_current_spots (worker/convertUUID "3KB:P(&oP5G@!P=on@JH"))
          nr (spy :debug (count spots))
          nr2 (count spots2)]
      (is (not= spots nil))
      (is (= nr 6))
      (is (= nr2 6)))
    (worker/process-device-msg msg2)
    (Thread/sleep 200)
    (let [spots (logger/get_latest_parking_info "ubersite")
          spots2 (cass-util/get_current_spots (worker/convertUUID "3KB:P(&oP5G@!P=on@JH"))
          nr (count spots)
          nr2 (count spots2)]
      (is (not= spots nil))
      (is (= nr 4))
      (is (= nr2 4)))
    (worker/process-device-msg msg3)
    (Thread/sleep 200)
    (let [spots (logger/get_latest_parking_info "ubersite")
          spots2 (cass-util/get_current_spots (worker/convertUUID "3KB:P(&oP5G@!P=on@JH"))
          nr (count spots)
          nr2 (count spots2)]
      (is (not= spots nil))
      (is (= nr 0))
      (is (= nr2 0)))
    ))

(deftest test-store-dem-parking-event
    (let [cfg {:roi    {:n     "Parking_Area_Z",
                   :img   [{:v [0.0,0.0,0.0,0.0],:u [0.0,0.0,0.0,0.0]}],
                   :uuid  "Ei[({v+jvmW$hmCp?WfB",
                   :vs    [5.0, 1.7999999523162842, 1.5],
                   :spots [{:img   {:v [0.0,0.0,0.0,0.0],:u [0.0,0.0,0.0,0.0]},
                            :e     true,
                            :uuid  "!Y.emJ-nxUYKi8fXnEAT",
                            :world {:lon [0, 0, 0, 0], :lat [0,0,0,0]}},
                           {:img   {:v [0.0,0.0,0.0,0.0], :u [0.0,0.0,0.0,0.0]},
                            :e     true,
                            :uuid  "![vQW7be2fS\\/AJ\\/vM}Rg",
                            :world {:lon [0, 0, 0, 0], :lat [0,0,0,0]}},
                           {:img   {:v [0.0,0.0,0.0,0.0], :u [0.0,0.0,0.0,0.0]},
                            :e     true,
                            :uuid  "1$RJ5Y+i:MIeJY5(\\/^B0",
                            :world {:lon [0, 0, 0, 0], :lat [0,0,0,0]}},
                          {:img   {:v [0.0,0.0,0.0,0.0], :u [0.0,0.0,0.0,0.0]},
                           :e     true,
                           :uuid  "8GeWQt4yjuY!n.eJZ>TJ",
                           :world {:lon [0, 0, 0, 0], :lat [0,0,0,0]}},
                           {:img   {:v [0.0,0.0,0.0,0.0], :u [0.0,0.0,0.0,0.0]},
                            :e     true,
                            :uuid  "C5f&cZunkyNv>8oA]2>*",
                            :world {:lon [0, 0, 0, 0], :lat [0,0,0,0]}},
                            {:img   {:v [0.0,0.0,0.0,0.0], :u [0.0,0.0,0.0,0.0]},
                             :e     true,
                             :uuid  "n[{U@C7rg}FCuxdFFs{K",
                             :world {:lon [0, 0, 0, 0], :lat [0,0,0,0]}}
                            ],
                   :world [{:lon [0, 0, 0, 0],
                            :lat [0, 0, 0, 0]}]},
          :h      {:n    "Parking Area 1",
                   :e    true,
                   :des  "",
                   :ch   0,
                   :uuid "3KB:P(&oP5G@!P=on@JH",
                   :tag  "",
                   :t    1496964335136000,
                   :u    "nr"}
          :nodeid "ubernode",
          :name   "DemarcatedParkingConfig"},
          evn    {:nodeid "ubernode",
                  :name   "DemarcatedParkingEvent",
                  :h      {:n    "Parking Area 1"
                           :e    true
                           :ch   1
                           :uuid "3KB:P(&oP5G@!P=on@JH"
                           :t    1507066318351542}
                  :occ {
                        "!Y.emJ-nxUYKi8fXnEAT"      ["jq(ZOo20-.PMX<FOOc\\/)"]
                        "![vQW7be2fS\\/AJ\\/vM}Rg"  ["snmaxdkBf@II{k4!8r2w"]
                        "1$RJ5Y+i:MIeJY5(\\/^B0"    ["ho5]X*]9\\/bM[pI^Vnj]4"]
                        "8GeWQt4yjuY!n.eJZ>TJ"      ["Ql]uc.1go}GWs6r0c+A-"]
                        "C5f&cZunkyNv>8oA]2>*"      ["Lm:rxD#8xqQ!VT#N]!7o"]
                        "n[{U@C7rg}FCuxdFFs{K"      ["QVnb1!vFD3X--lb-=2Sc"]
                        ;:spotuuids ["!Y.emJ-nxUYKi8fXnEAT","![vQW7be2fS\\/AJ\\/vM}Rg","1$RJ5Y+i:MIeJY5(\\/^B0","8GeWQt4yjuY!n.eJZ>TJ","C5f&cZunkyNv>8oA]2>*","n[{U@C7rg}FCuxdFFs{K"]
                        ;:objectuuids [["jq(ZOo20-.PMX<FOOc\\/)"]["snmaxdkBf@II{k4!8r2w"]["ho5]X*]9\\/bM[pI^Vnj]4"]["Ql]uc.1go}GWs6r0c+A-"]["Lm:rxD#8xqQ!VT#N]!7o"]["QVnb1!vFD3X--lb-=2Sc"]]
                        }
                  :o      [{:iv    [0.0 0.0]
                            :s     1507060635000000
                            :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                            :c     "Car"
                            :wh    1.6779011487960815
                            :wv    [0.0 0.0]
                            :uuid  "jq(ZOo20-.PMX<FOOc\\/)"
                            :wp    1.0
                            :img   {:v [0 0] :u [0 0]}},
                           {:iv    [0.0 0.0]
                            :s     1507061720000000
                            :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                            :c     "Car"
                            :wh    1.6779011487960815
                            :wv    [0.0 0.0]
                            :uuid  "snmaxdkBf@II{k4!8r2w"
                            :wp    1.0
                            :img   {:v [0 0] :u [0 0]}},
                           {:iv    [0.0 0.0]
                            :s     1507065557000000
                            :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                            :c     "Car"
                            :wh    1.6779011487960815
                            :wv    [0.0 0.0]
                            :uuid  "ho5]X*]9\\/bM[pI^Vnj]4"
                            :wp    1.0
                            :img   {:v [0 0] :u [0 0]}},
                           {:iv    [0.0 0.0]
                            :s     1507065563000000
                            :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                            :c     "Car"
                            :wh    1.6779011487960815
                            :wv    [0.0 0.0]
                            :uuid  "Ql]uc.1go}GWs6r0c+A-"
                            :wp    1.0
                            :img   {:v [0 0] :u [0 0]}},
                           {:iv    [0.0 0.0]
                            :s     1507065756000000
                            :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                            :c     "Car"
                            :wh    1.6779011487960815
                            :wv    [0.0 0.0]
                            :uuid  "Lm:rxD#8xqQ!VT#N]!7o"
                            :wp    1.0
                            :img   {:v [0 0] :u [0 0]}},
                           {:iv    [0.0 0.0]
                            :s     1507066169000000
                            :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                            :c     "Car"
                            :wh    1.6779011487960815
                            :wv    [0.0 0.0]
                            :uuid  "QVnb1!vFD3X--lb-=2Sc"
                            :wp    1.0
                            :img   {:v [0 0] :u [0 0]}}
                          ]},
          evn2    {:nodeid "ubernode"
                  ,
                  :name   "DemarcatedParkingEvent"
                  ,
                  :occ {
                        ;:spotuuids [] :objectuuids []
                        }
                  :h      {:n    "Parking Area 1"
                           :e    true
                           :ch   1
                           :uuid "3KB:P(&oP5G@!P=on@JH"
                           :t    1507066333081910}
                  :o      []}
          evn1    {:nodeid "ubernode"
                  ,
                  :name   "DemarcatedParkingEvent"
                  ,
                  :occ {
                        ;:spotuuids [] :objectuuids []
                        }
                  :h      {:n    "Parking Area 1"
                           :e    true
                           :ch   1
                           :uuid "3KB:P(&oP5G@!P=on@JH"
                           :t    1507066303005858}
                  :o      []}
          evn3   {:nodeid "ubernode",
                  :name   "DemarcatedParkingEvent",
                  :h      {:n    "Parking Area 1"
                           :e    true
                           :ch   1
                           :uuid "3KB:P(&oP5G@!P=on@JH"
                           :t    1507066348410114}
                  :occ {
                        "!Y.emJ-nxUYKi8fXnEAT"      ["jq(ZOo20-.PMX<FOOc\\/)"]
                        "![vQW7be2fS\\/AJ\\/vM}Rg"  ["snmaxdkBf@II{k4!8r2w"]
                        "1$RJ5Y+i:MIeJY5(\\/^B0"    ["ho5]X*]9\\/bM[pI^Vnj]4"]
                        "8GeWQt4yjuY!n.eJZ>TJ"      ["Ql]uc.1go}GWs6r0c+A-"]
                        "C5f&cZunkyNv>8oA]2>*"      ["Lm:rxD#8xqQ!VT#N]!7o"]
                        "n[{U@C7rg}FCuxdFFs{K"      ["QVnb1!vFD3X--lb-=2Sc"]
                        ;:spotuuids ["!Y.emJ-nxUYKi8fXnEAT","![vQW7be2fS\\/AJ\\/vM}Rg","1$RJ5Y+i:MIeJY5(\\/^B0","8GeWQt4yjuY!n.eJZ>TJ","C5f&cZunkyNv>8oA]2>*","n[{U@C7rg}FCuxdFFs{K"]
                        ;:objectuuids [["jq(ZOo20-.PMX<FOOc\\/)"]["snmaxdkBf@II{k4!8r2w"]["ho5]X*]9\\/bM[pI^Vnj]4"]["Ql]uc.1go}GWs6r0c+A-"]["Lm:rxD#8xqQ!VT#N]!7o"]["QVnb1!vFD3X--lb-=2Sc"]]
                        }
                  :o      [{:iv    [0.0 0.0]
                            :s     1507060635000000
                            :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                            :c     "Car"
                            :wh    1.6779011487960815
                            :wv    [0.0 0.0]
                            :uuid  "jq(ZOo20-.PMX<FOOc\\/)"
                            :wp    1.0
                            :img   {:v [0 0] :u [0 0]}},
                           {:iv    [0.0 0.0]
                            :s     1507061720000000
                            :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                            :c     "Car"
                            :wh    1.6779011487960815
                            :wv    [0.0 0.0]
                            :uuid  "snmaxdkBf@II{k4!8r2w"
                            :wp    1.0
                            :img   {:v [0 0] :u [0 0]}},
                           {:iv    [0.0 0.0]
                            :s     1507065557000000
                            :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                            :c     "Car"
                            :wh    1.6779011487960815
                            :wv    [0.0 0.0]
                            :uuid  "ho5]X*]9\\/bM[pI^Vnj]4"
                            :wp    1.0
                            :img   {:v [0 0] :u [0 0]}},
                           {:iv    [0.0 0.0]
                            :s     1507065563000000
                            :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                            :c     "Car"
                            :wh    1.6779011487960815
                            :wv    [0.0 0.0]
                            :uuid  "Ql]uc.1go}GWs6r0c+A-"
                            :wp    1.0
                            :img   {:v [0 0] :u [0 0]}},
                           {:iv    [0.0 0.0]
                            :s     1507065756000000
                            :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                            :c     "Car"
                            :wh    1.6779011487960815
                            :wv    [0.0 0.0]
                            :uuid  "Lm:rxD#8xqQ!VT#N]!7o"
                            :wp    1.0
                            :img   {:v [0 0] :u [0 0]}},
                           {:iv    [0.0 0.0]
                            :s     1507066169000000
                            :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                            :c     "Car"
                            :wh    1.6779011487960815
                            :wv    [0.0 0.0]
                            :uuid  "QVnb1!vFD3X--lb-=2Sc"
                            :wp    1.0
                            :img   {:v [0 0] :u [0 0]}}
                          ]}
                 evn4 {:nodeid "ubernode",
                         :name   "DemarcatedParkingEvent",
                         :h      {:n    "Parking Area 1"
                                  :e    true
                                  :ch   1
                                  :uuid "3KB:P(&oP5G@!P=on@JH"
                                  :t    1507066355461492}
                         :occ {
                               "!Y.emJ-nxUYKi8fXnEAT"      ["jq(ZOo20-.PMX<FOOc\\/)"]
                               "1$RJ5Y+i:MIeJY5(\\/^B0"    ["ho5]X*]9\\/bM[pI^Vnj]4"]
                               "8GeWQt4yjuY!n.eJZ>TJ"      ["Ql]uc.1go}GWs6r0c+A-"]
                               "C5f&cZunkyNv>8oA]2>*"      ["Lm:rxD#8xqQ!VT#N]!7o"]
                               "n[{U@C7rg}FCuxdFFs{K"      ["QVnb1!vFD3X--lb-=2Sc"]
                               ;:spotuuids ["!Y.emJ-nxUYKi8fXnEAT","1$RJ5Y+i:MIeJY5(\\/^B0","8GeWQt4yjuY!n.eJZ>TJ","C5f&cZunkyNv>8oA]2>*","n[{U@C7rg}FCuxdFFs{K"]
                               ;:objectuuids [["jq(ZOo20-.PMX<FOOc\\/)"]["ho5]X*]9\\/bM[pI^Vnj]4"]["Ql]uc.1go}GWs6r0c+A-"]["Lm:rxD#8xqQ!VT#N]!7o"]["QVnb1!vFD3X--lb-=2Sc"]]
                               }
                         :o      [{:iv    [0.0 0.0]
                                   :s     1507060635000000
                                   :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                                   :c     "Car"
                                   :wh    1.6779011487960815
                                   :wv    [0.0 0.0]
                                   :uuid  "jq(ZOo20-.PMX<FOOc\\/)"
                                   :wp    1.0
                                   :img   {:v [0 0] :u [0 0]}},
                                  {:iv    [0.0 0.0]
                                   :s     1507061720000000
                                   :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                                   :c     "Car"
                                   :wh    1.6779011487960815
                                   :wv    [0.0 0.0]
                                   :uuid  "ho5]X*]9\\/bM[pI^Vnj]4"
                                   :wp    1.0
                                   :img   {:v [0 0] :u [0 0]}},
                                  {:iv    [0.0 0.0]
                                   :s     1507065557000000
                                   :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                                   :c     "Car"
                                   :wh    1.6779011487960815
                                   :wv    [0.0 0.0]
                                   :uuid  "Ql]uc.1go}GWs6r0c+A-"
                                   :wp    1.0
                                   :img   {:v [0 0] :u [0 0]}},
                                  {:iv    [0.0 0.0]
                                   :s     1507065562000000
                                   :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                                   :c     "Car"
                                   :wh    1.6779011487960815
                                   :wv    [0.0 0.0]
                                   :uuid  "Lm:rxD#8xqQ!VT#N]!7o"
                                   :wp    1.0
                                   :img   {:v [0 0] :u [0 0]}},
                                  {:iv    [0.0 0.0]
                                   :s     1507066169000000
                                   :world {:lon [0 0 0 0] :lat [0 0 0 0]}
                                   :c     "Car"
                                   :wh    1.6779011487960815
                                   :wv    [0.0 0.0]
                                   :uuid  "QVnb1!vFD3X--lb-=2Sc"
                                   :wp    1.0
                                   :img   {:v [0 0] :u [0 0]}}
                                 ]}

          msg0   (msgpk/pack (stringify-keys cfg))
          msg1   (msgpk/pack (stringify-keys evn1))
          msg    (msgpk/pack (stringify-keys evn))
          msg2   (msgpk/pack (stringify-keys evn2))
          msg3   (msgpk/pack (stringify-keys evn3))
          msg4   (msgpk/pack (stringify-keys evn4))
          logmsg (atom [])]
      (worker/process-device-msg msg0)
      (Thread/sleep 200)
      (worker/process-device-msg msg1)
      (Thread/sleep 200)
      (worker/process-device-msg msg)
      (Thread/sleep 200)
      (let [spots (logger/get_latest_parking_info "ubersite")
            nr (spy :debug (count spots))]
        (is (not= spots nil))
        (is (= (:occupancy (nth spots 0)) true))
        (is (= nr 6)))
      (worker/process-device-msg msg2)
      (worker/process-device-msg msg3)
      (worker/process-device-msg msg4)

      (let [spots (logger/get_latest_parking_info "ubersite")
            nr (spy :debug (count spots))]
        (is (not= spots nil))
        (is (= (:occupancy (nth spots 0)) true))
        (is (= nr 6)))))

(deftest test-store-parking-event
  (let [evn    {:nodeid "ubernode"
                ,
                :name   "NonDemarcatedParkingEvent"
                ,
                :h      {:n    "Parking Area 1"
                         :e    true
                         :ch   1
                         :uuid "3KB:P(&oP5G@!P=on@JH"
                         :t    1496680571357386}
                :o      [{:iv    [0.0 0.0]
                          :s     1496680392000000
                          :world {:lon [-87.62228624581819 -87.62228369784921 -87.62226085629469 -87.62226340426511]
                                  :lat [41.88764281742284 41.88759249824228 41.8875931440505 41.887643463231065]}
                          :c     "Car"
                          :wh    1.6779011487960815
                          :wv    [0.0 0.0]
                          :uuid  "q#%tMWvZe>R!OxCAx${V"
                          :wp    1.0
                          :img   {:v [0.5944444537162781 0.8041666746139526]
                                  :u [0.19765624403953552 0.41718751192092896]}}]}
        evn2    {:nodeid "ubernode"
                ,
                :name   "NonDemarcatedParkingEvent"
                ,
                :h      {:n    "Parking Area 1"
                         :e    true
                         :ch   1
                         :uuid "3KB:P(&oP5G@!P=on@JH"
                         :t    1496680571357386}
                :o      []}


        msg    (msgpk/pack (stringify-keys evn))
        msg2   (msgpk/pack (stringify-keys evn2))
        logmsg (atom [])]

    (worker/process-device-msg msg)
    (Thread/sleep 200)
    (let [configs (logger/get_latest_parking_info "ubersite")
          nr (spy :debug (count configs))]
      (is (not= configs nil))
      (is (= nr 1)))
    (worker/process-device-msg msg2)

    (let [configs (logger/get_latest_parking_info "ubersite")
          nr (spy :debug (count configs))]
      (is (not= configs nil))
      (is (= nr 0)))))

(deftest test-store-parking-config
  (let [
         cfg1 {
                :roi  {
                        :n "Roi 1",
                        :img [
                               {:v [0.05555124953389168,0.05555124953389168],
                                :u [0.660639762878418,0.813434362411499]}
                               ],
                        :uuid "Yi2@MO6lVnPqS9Wk^}9$",
                        :world [
                                 {:lon [-121.992805,-121.992805],
                                  :lat [37.38089,37.380844]}
                                 ]
                        },
                ;:removed [],
                :delta "",
                :nodeid "ubernode",
                :name "NonDemarcatedParkingConfig",
                :h {
                     :n "ndpark",
                     :e true,
                     :des "",
                     :ch 0,
                     :uuid "P4bZ8*T>cokT:Z?Rznt{",
                     :tag [""],
                     :t 1476701656210080,
                     :u ""
                     }
                }
         cfg2 {
                :roi  {
                        :n "Roi 1",
                        :img [
                               {:v [0.05555124953389168,0.05555124953389168],
                                :u [0.660639762878418,0.813434362411499]}
                               ],
                        :uuid "Yi2@MO6lVnPqS9Wk^}9$",
                        :world [
                                 {:lon [-121.992805,-121.992805],
                                  :lat [37.38089,37.380844]}
                                 ]
                        },
                ;:removed [],
                :delta "",
                :nodeid "ubernode",
                :name "NonDemarcatedParkingConfig",
                :h {
                     :n "ndpark",
                     :e false,
                     :des "",
                     :ch 0,
                     :uuid "P4bZ8*T>cokT:Z?Rznt{",
                     :tag [""],
                     :t 1476701656210081,
                     :u ""
                     }
                }
        cfg3 {
               ;:removed [],
               :delta "",
               :nodeid "ubernode",
               :name "NonDemarcatedParkingConfig",
               :h {
                    :n "ndpark",
                    :e false,
                    :des "",
                    :ch 0,
                    :uuid "P4bZ8*T>cokT:Z?Rznt{",
                    :tag [""],
                    :t 1476701656210081,
                    :u ""
                    }
               }

         msg1 (msgpk/pack (stringify-keys cfg1))
         msg2 (msgpk/pack (stringify-keys cfg2))
         msg3 (msgpk/pack (stringify-keys cfg3))
         logmsg (atom [])]

      ;(spy :error (logger/get_latest_parking_info "ubersite"))
      (worker/process-device-msg msg1)
      ;(spy :error (logger/get_latest_parking_info "ubersite"))
      (worker/process-device-msg msg2)
      ;(spy :error (logger/get_latest_parking_info "ubersite"))
      (worker/process-device-msg msg3)
      ;(spy :error (logger/get_latest_parking_info "ubersite"))
      (let [
             configs (logger/get_latest_parking_info "ubersite")
             nr (spy :debug (count configs))]
        (is (not= configs nil))
        (is (= 0 nr)))
))




(deftest test-publish-parking-config-event
  (let [
         cfg1 {
                :roi  {
                        :n "Roi 1",
                        :img [
                               {:v [0.05555124953389168,0.05555124953389168],
                                :u [0.660639762878418,0.813434362411499]}
                               ],
                        :uuid "Yi2@MO6lVnPqS9Wk^}9$",
                        :world [
                                 {:lon [-121.992805,-121.992805],
                                  :lat [37.38089,37.380844]}
                                 ]
                        },
                ;:removed [],
                :delta "",
                :nodeid "ubernode",
                :name "NonDemarcatedParkingConfig",
                :h {
                     :n "ndpark",
                     :e true,
                     :des "",
                     :ch 0,
                     :uuid "P4bZ8*T>cokT:Z?Rznt{",
                     :tag [""],
                     :t 1476701656210080,
                     :u ""
                     }
                }
         evn1 {
                :roi  {
                        :n "Roi 1",
                        :img [
                               {:v [0.05555124953389168,0.05555124953389168],
                                :u [0.660639762878418,0.813434362411499]}
                               ],
                        :uuid "Yi2@MO6lVnPqS9Wk^}9$",
                        :world [
                                 {:lon [-121.992805,-121.992805],
                                  :lat [37.38089,37.380844]}
                                 ]
                        },
                ;:removed [],
                :delta "",
                :nodeid "ubernode",
                :name "NonDemarcatedParkingEvent",
                :h {
                     :n "ndpark",
                     :e false,
                     :des "",
                     :ch 0,
                     :uuid "P4bZ8*T>cokT:Z?Rznt{",
                     :tag [""],
                     :t 1476701656210081,
                     :u ""
                     }
                }

         evn2        {:nodeid "ubernode",
                      :name   "NonDemarcatedParkingEvent",
                      :h      {:n    "Parking Area 2",
                               :e    true,
                               :ch   0,
                               :uuid "2$:IJo1mqTYV&a>7l]&u",
                               :t    1476701656210081},
                      :o      [{:iv    [0.0 0.0],
                                :s     1496419106000000,
                                :world [{:lon [-94.58059704674618 -94.58058043737498 -94.58055806936541 -94.58057467873661],
                                        :lat [39.10936272486224 39.10931334180431 39.10931790830632 39.10936729136426]}],
                                :c     "Car",
                                :wh    1.700244665145874,
                                :wv    [0.0 0.0],
                                :uuid  "WKp^C@<>JiRn6RU>3$A}",
                                :wp    0.5,
                                :img   [{:v [0.18333333730697632 0.4263888895511627],
                                        :u [0.5609375238418579 0.8453124761581421]}]}
                                {:iv    [-0.0070095062255859375 -0.0298394076526165],
                                 :s     1493482521000000,
                                 :world [{:lon [-94.58061939607771 -94.58060422608493 -94.58058394187951 -94.58059911187229],
                                         :lat [39.1094136814747 39.109369783359625 39.1093740381539 39.109417936268976]}],
                                 :c     "Car",
                                 :wh    1.5141006708145142,
                                 :wv    [0.4659779965877533 0.8497915267944336],
                                 :uuid  "YDYEcd8!LxU$Mu)(p+5M",
                                 :wp    1.0,
                                 :img   [{:v [0.22361111640930176 0.4027777910232544],
                                         :u [0.3453125059604645 0.553906261920929]}]}]}
         msg1 (msgpk/pack (stringify-keys cfg1))
         msg2 (msgpk/pack (stringify-keys evn1))
         msg3 (msgpk/pack (stringify-keys evn2))
         logmsg (atom [])
         mqtt-update (atom {})]
    (defn get-mqtt-map [msgmap]
      (->
        msgmap
        (assoc :orgid "uberorg" :nodeid "ubernode" :siteid "ubersite")
        (rename-keys {:alarmType :type
                      :alarmSeverity :severity})))
    (defn get-mqtt-msg []
      (->
        (:msg @mqtt-update)
        (json/read-str :key-fn keyword)
        (dissoc :alertid)))
    (with-redefs [mqttcore/mqtt-publish #(swap! mqtt-update assoc :topic %1 :msg %2 :client-id %3)]
      (worker/process-device-msg msg1)
      (let [
             configs (logger/get_latest_parking_info "ubersite")
             ]
        (is (= (:configured_date (get-mqtt-msg)) "2016-10-17 10:54:16.21")))
      (worker/process-device-msg msg2)
      (let [
             configs (logger/get_latest_parking_info "ubersite")
             ]
        (is (= (:time (get-mqtt-msg)) "2016-10-17 10:54:16.21")))
;      (worker/process-device-msg msg3)
;      (let [
;             evens (logger/get_latest_parking_info "ubersite")
;             ]
;        (is (= (:time (get-mqtt-msg)) "2016-10-17 10:54:16.21")))
      )))

(deftest test-process-traffic-event
  (let [ ev1 {
               :c 13827,
               :co {
                    :car 13827
               }
               :h {
                    :ch 0,
                    :e true,
                    :n "Event 1",
                    :t 1492431477682496,
                    :uuid "?/Dm7=yvo>NQvp[eQ=7$"
                    },
               :name "ObjectEnteringEvent",
               :nodeid "ubernode",
               :o [
                    {
                      :c "Car",
                      :co "Blue"
                      :img {
                             :u [0.19999998807907104, 0.375],
                             :v [0.2874999940395355, 0.4763889014720917]
                             },
                      :iv [0.07223708927631378, -0.01373439934104681],
                      :uuid "W+CONSZom<X7zS9>OrU(",
                      :wh 2.878308057785034,
                      :world {
                               :lat [41.88760977510776, 41.88756715245182, 41.88756715245182, 41.88760977510776],
                               :lon [-87.62230606482949, -87.62230606482949, -87.62236310519815 -87.62236310519815]},
                      :wp 1.0,
                      :wv [-0.1757669448852539, 2.963390350341797]
                      }
                    ]
               }
          ev2 { :nodeid "ubernode"
                :name "LineCrossingEvent"
                :h {
                     :n "Event 2"
                     :e true
                     :ch 0
                     :uuid ")ghW$=dvJ0V6I49l.PaZ"
                     :t 1477653597421994}
                :o [{
                      :iv [0.0 0.0],
                      :img {:v [0.0 0.0],
                            :u [0.0 0.0]},
                      :wv [3.294614327242327 4.25864519847656],
                      :uuid "LgU/i&L=6[TVs?xCw7>S",
                      :wp 1.0,
                      :wh 3.0,
                      :c "car",
                      :world {
                               :lon [0.0 0.0],
                               :lat [0.0 0.0]}}]
                :c 12982}
          ev3 {
                :nodeid "ubernode"
                :name "ObjectLeavingEvent",
                :h {
                     :n "Event 3",
                     :e true,
                     :ch 0,
                     :uuid "$W89P7Ad^lFgw+(CBn%I",
                     :t 1477653927491598},
                :o [{
                      :iv [0.0 0.0],
                      :img {
                             :v [0.0 0.0],
                             :u [0.0 0.0]},
                      :wv [0.0 0.0],
                      :uuid "EJt(#6%SrVnNeRkv(*FG",
                      :wp 1.0,
                      :wh 3.0,
                      :c "car",
                      :world {
                               :lon [0.0 0.0],
                               :lat [0.0 0.0]}}]
                :c 7250}
          cfg1 {
                 :roi  {
                         :n "",
                         :img [
                                {:v [0.05555124953389168,0.05555124953389168],
                                 :u [0.660639762878418,0.813434362411499]}
                                ],
                         :uuid "Yi2@MO6lVnPqS9Wk^}9$",
                         :world [
                                  {:lon [-121.992805,-121.992805],
                                   :lat [37.38089,37.380844]}
                                  ]
                         },
                 ;:removed [],
                 :delta "",
                 :nodeid "ubernode",
                 :name "LineCrossingConfig",
                 :h {
                      :n "linec",
                      :e true,
                      :des "",
                      :ch 0,
                      :uuid "P4bZ8*T>cokT:Z?Rznt{",
                      :tag [""],
                      :t 1476701656210080,
                      :u ""
                      }
                 }
        msg1 (msgpk/pack (stringify-keys ev1))
        msg2 (msgpk/pack (stringify-keys ev2))
        msg3 (msgpk/pack (stringify-keys ev3))
        logmsg (atom [])]

    (with-redefs [logger/log_traffic_current_status #(swap! logmsg conj %1)]
      (worker/process-device-msg msg1)
      (worker/process-device-msg msg2)
      (worker/process-device-msg msg3)


      (is (= (map #(assoc % :data (json/read-str (:data %) :key-fn keyword)) @logmsg) [
                       {:orgid "uberorg",
                        :nodeid "ubernode",
                        :trafficdetectioneventid "DF73B6FC-CE9C-4445-9A60-21862D7DD32A",
                        :type "ObjectEnteringEvent",
                        :name "Event 1",
                        :siteid "ubersite",
                        :time 1492431477682496,
                        :channel 0,
                        :active true,
                        :count 13827,
                        :data {:orgid "uberorg",
                               :nodeid "ubernode",
                               :count_per_class {:car 13827},
                               :detected_objects [{:detectedobjectid "B6DB6B88-AA42-48BE-B7D8-1B08E815F25A", :world_velocity {:x -0.17576694, :y 2.9633904}, :world_bounding_box [{:latitude 41.88761, :longitude -87.62231} {:latitude 41.887566, :longitude -87.62231} {:latitude 41.887566, :longitude -87.62236} {:latitude 41.88761, :longitude -87.62236}], :color "Blue", :image_bounding_box [{:x 0.2875, :y 0.19999999} {:x 0.4763889, :y 0.375}], :image_velocity {:x 0.07223709, :y -0.013734399}, :class "Car", :position_precision 1.0, :height 2.878308}],
                               :trafficdetectioneventid "DF73B6FC-CE9C-4445-9A60-21862D7DD32A",
                               :name "Event 1",
                               :siteid "ubersite",
                               :time "2017-04-17 12:17:57.68",
                               :channel 0,
                               :type "ObjectEnteringEvent",
                               :active true,
                               :count 13827}}
                       {:orgid "uberorg",
                        :nodeid "ubernode",
                        :trafficdetectioneventid "ED0F46BA-CDD7-8183-B196-A02043A182D5",
                        :type "LineCrossingEvent",
                        :name "Event 2",
                        :siteid "ubersite",
                        :time 1477653597421994,
                        :channel 0,
                        :active true,
                        :count 12982,
                        :data {:orgid "uberorg",
                               :nodeid "ubernode",
                               :count_per_class nil,
                               :detected_objects [{:detectedobjectid "92D88EC2-E1C5-0848-AD39-D2BC77686E3D", :world_velocity {:x 3.2946143, :y 4.258645}, :world_bounding_box [{:latitude 0.0, :longitude 0.0} {:latitude 0.0, :longitude 0.0}], :color nil, :image_bounding_box [{:x 0.0, :y 0.0} {:x 0.0, :y 0.0}], :image_velocity {:x 0.0, :y 0.0}, :class "car", :position_precision 1.0, :height 3.0}],
                               :trafficdetectioneventid "ED0F46BA-CDD7-8183-B196-A02043A182D5",
                               :name "Event 2",
                               :siteid "ubersite",
                               :time "2016-10-28 11:19:57.42",
                               :channel 0,
                               :type "LineCrossingEvent",
                               :active true,
                               :count 12982}}
                       {:orgid "uberorg",
                        :nodeid "ubernode",
                        :trafficdetectioneventid "045F6C0D-171A-7F64-802A-CC2977990F0C",
                        :type "ObjectLeavingEvent",
                        :name "Event 3",
                        :siteid "ubersite",
                        :time 1477653927491598,
                        :channel 0,
                        :active true,
                        :count 7250,
                        :data {:orgid "uberorg",
                               :nodeid "ubernode",
                               :count_per_class nil,
                               :detected_objects [{:detectedobjectid "7E1DB701-15B1-8146-495C-B40F633AA63B", :world_velocity {:x 0.0, :y 0.0}, :world_bounding_box [{:latitude 0.0, :longitude 0.0} {:latitude 0.0, :longitude 0.0}], :color nil, :image_bounding_box [{:x 0.0, :y 0.0} {:x 0.0, :y 0.0}], :image_velocity {:x 0.0, :y 0.0}, :class "car", :position_precision 1.0, :height 3.0}],
                               :trafficdetectioneventid "045F6C0D-171A-7F64-802A-CC2977990F0C",
                               :name "Event 3",
                               :siteid "ubersite",
                               :time "2016-10-28 11:25:27.49",
                               :channel 0,
                               :type "ObjectLeavingEvent",
                               :active true,
                               :count 7250}
                        }]
             ))
      )))

(deftest test-process-traffic-config
  (let [
          cfg1 {
                 :roi  {
                         :n "Roi 1",
                         :img [
                                {:v [0.05555124953389168,0.05555124953389168],
                                 :u [0.660639762878418,0.813434362411499]}
                                {:v [0.05555134953389168,0.05555324953389168],
                                 :u [0.660639362878418,0.813434332411499]}
                                ],
                         :uuid "Yi2@MO6lVnPqS9Wk^}9$",
                         :world [
                                  {:lon [-121.992805,-121.992805],
                                   :lat [37.38089,37.380844]}
                                  {:lon [-121.992805,-121.992875],
                                   :lat [37.38087,37.380874]}
                                  ]
                         },
                 ;:removed [],
                 :delta "",
                 :nodeid "ubernode",
                 :name "LineCrossingConfig",
                 :tr 0
                 :h {
                      :n "linec",
                      :e true,
                      :des "",
                      :ch 0,
                      :uuid "P4bZ8*T>cokT:Z?Rznt{",
                      :tag [""],
                      :t 1476701656210080,
                      :u "nr"
                      }
                 }

         msg1 (msgpk/pack (stringify-keys cfg1))
         logmsg (atom [])]

    (with-redefs [logger/log_traffic_config #(swap! logmsg conj %1)]
      (worker/process-device-msg msg1)
      (is (= @logmsg [{:orgid "uberorg",
                       :nodeid "ubernode",
                       :name "linec",
                       :type "LineCrossingConfig",
                       :siteid "ubersite",
                       :time 1476701656210080,
                       :channel 0,
                       :eventid "9ED535B3-DBD7-D9DF-4044-E587A632061B",
                       :active true,
                       :data "{\"description\":\"\",\"orgid\":\"uberorg\",\"nodeid\":\"ubernode\",\"name\":\"linec\",\"siteid\":\"ubersite\",\"tr\":0,\"channel\":0,\"type\":\"LineCrossingConfig\",\"eventid\":\"9ED535B3-DBD7-D9DF-4044-E587A632061B\",\"active\":true,\"tag\":[\"\"],\"configured_date\":\"2016-10-17 10:54:16.21\",\"roi\":{\"roiid\":\"BB5819AD-9BCC-87D1-9FA8-0A9240B70AFB\",\"name\":\"Roi 1\",\"image_bounding_box\":[[{\"x\":0.05555125,\"y\":0.66063976},{\"x\":0.05555125,\"y\":0.81343436}],[{\"x\":0.05555135,\"y\":0.66063935},{\"x\":0.05555325,\"y\":0.8134343}]],\"world_bounding_box\":[[{\"latitude\":37.38089,\"longitude\":-121.992805},{\"latitude\":37.380844,\"longitude\":-121.992805}],[{\"latitude\":37.38087,\"longitude\":-121.992805},{\"latitude\":37.380875,\"longitude\":-121.992874}]],\"vs\":null},\"user\":\"nr\"}"}]))
      )))

(deftest test-store-traffic-config
  (let [
         cfg1 {
                :roi  {
                        :n "Roi 1",
                        :img [
                               {:v [0.05555124953389168,0.05555124953389168],
                                :u [0.660639762878418,0.813434362411499]}
                               ],
                        :uuid "Yi2@MO6lVnPqS9Wk^}9$",
                        :world [
                                 {:lon [-121.992805,-121.992805],
                                  :lat [37.38089,37.380844]}
                                 ]
                        },
                ;:removed [],
                :delta "",
                :nodeid "ubernode",
                :name "LineCrossingConfig",
                :tr 0
                :h {
                     :n "linec",
                     :e false,
                     :des "",
                     :ch 0,
                     :uuid "P4bZ8*T>cokT:Z?Rznt{",
                     :tag [""],
                     :t 1476701656210080,
                     :u ""
                     }
                }
         cfg2 {
                :roi  {
                        :n "Roi 1",
                        :img [
                               {:v [0.05555124953389168,0.05555124953389168],
                                :u [0.660639762878418,0.813434362411499]}
                               ],
                        :uuid "Yi2@MO6lVnPqS9Wk^}9$",
                        :world [
                                 {:lon [-121.992805,-121.992805],
                                  :lat [37.38089,37.380844]}
                                 ]
                        },
                ;:removed [],
                :delta "",
                :nodeid "ubernode",
                :name "LineCrossingConfig",
                :tr 1
                :h {
                     :n "Pedestrian.Count",
                     :e true,
                     :des "",
                     :ch 0,
                     :uuid "31B@OO0lqPO8Y>[VR8oc",
                     :tag [""],
                     :t 1476701656210081,
                     :u ""
                     }
                }

         msg1 (msgpk/pack (stringify-keys cfg1))
         msg2 (msgpk/pack (stringify-keys cfg2))
         logmsg (atom [])]


      (worker/process-device-msg msg1)
      (worker/process-device-msg msg2)
      (let [
             configs (logger/get_traffic_config "ubersite" "ubernode" {:eventid "9ED535B3-DBD7-D9DF-4044-E587A632061B"}  )]
        (is (= (first configs)
               {"roi" {
                  "roiid" "BB5819AD-9BCC-87D1-9FA8-0A9240B70AFB",
                  "name" "Roi 1",
                  "image_bounding_box" [[
                    {"x" 0.05555125, "y" 0.66063976}
                    {"x" 0.05555125, "y" 0.81343436}]],
                  "world_bounding_box" [[
                     {"latitude" 37.38089, "longitude" -121.992805}
                     {"latitude" 37.380844, "longitude" -121.992805}]]
                  "vs" nil
               },
               "user" "",
               "nodeid" "ubernode",
               "tr" 0
               "type" "LineCrossingConfig",
               "tag" [""],
               "eventid" "9ED535B3-DBD7-D9DF-4044-E587A632061B",
               "configured_date" "2016-10-17 10:54:16.21",
               "siteid" "ubersite",
               "name" "linec",
               "channel" 0,
               "active" false,
               "description" "",
               "orgid" "uberorg"
               })))
))

(deftest test-process-all-config
  (let [

         cfg1 {
                :roi  {
                        :n "Roi 1",
                        :img [
                               {:v [0.05555124953389168,0.05555124953389168],
                                :u [0.660639762878418,0.813434362411499]}
                               ],
                        :uuid "Yi2@MO6lVnPqS9Wk^}9$",
                        :world [
                                 {:lon [-121.992805,-121.992805],
                                  :lat [37.38089,37.380844]}
                                 ]
                        },
                ;:removed [],
                :delta "",
                :nodeid "ubernode",
                :name "LineCrossingConfig",
                :h {
                     :n "Pedestrian.Count",
                     :e true,
                     :des "",
                     :ch 0,
                     :uuid "31B@OO0lqPO8Y>[VR8oc",
                     :tag [""],
                     :t 1476701656210081,
                     :u ""
                     }
                }

         ev1 { :nodeid "ubernode"
               :name "LineCrossingEvent"
               :h {
                    :n "Pedestrian.Count"
                    :e true
                    :ch 0
                    :uuid "31B@OO0lqPO8Y>[VR8oc"
                    :t 1476701656210085}
               :o [{
                     :iv [0.0 0.0],
                     :img {:v [0.0 0.0],
                           :u [0.0 0.0]},
                     :wv [3.294614327242327 4.25864519847656],
                     :uuid "LgU/i&L=6[TVs?xCw7>S",
                     :wp 1.0,
                     :wh 3.0,
                     :c "person",
                     :world {
                              :lon [0.0 0.0],
                              :lat [0.0 0.0]}}]
               :c 12982}
         cfg2 {:roi    {:n     "Parking_Area_1",
                        :img   [{:v [0.34626907110214233,
                                     0.15447795391082764,
                                     0.18441490828990936,
                                     0.35074955224990845],
                                 :u [0.12549659609794617,
                                     0.20654840767383575,
                                     0.32884645462036133,
                                     0.3006282448768616]}],
                        :uuid  "Ei[({v+jvmW$hmCp?WfB",
                        :vs    [5.0, 1.7999999523162842, 1.5],
                        :spots [{:img   {:v [0.09406828135251999,
                                             0.09406828135251999,
                                             0.34365329146385193,
                                             0.34365329146385193],
                                         :u [0.18264101445674896,
                                             0.29642608761787415,
                                             0.29642608761787415,
                                             0.18264101445674896]},
                                 :e     true,
                                 :uuid  "P>TtR]N6t6G2bWyle0bW",
                                 :world {:lon [138.60647761850913, 138.60652918874038, 138.60653577727422, 138.60648420704297],
                                         :lat [-34.924735570660815,
                                               -34.92475064414831,
                                               -34.92473535337679,
                                               -34.92472027988875]}}],
                        :world [{:lon [138.60647453422987, 138.6065403808459, 138.60653501614593, 138.60647686033457],
                                 :lat [-34.92470793826165, -34.92472803160142, -34.92476101479756, -34.92474486341323]}]},
               :h      {:n    "DM Parking Area 1",
                        :e    true,
                        :des  "",
                        :ch   0,
                        :uuid "Gytxh]o-idKh$1SG0*JE",
                        :tag  "",
                        :t    1496964335136000,
                        :u    "nr"}
               :nodeid "ubernode",
               :name   "DemarcatedParkingConfig"},
         cfg3 {:roi    {:n     "Parking_Area_1",
                        :img   [{:v [0.5479455590248108, 0.5868344902992249, 0.8423900008201599, 0.7423900365829468],
                                 :u [0.23967008292675018,
                                     0.45217007398605347,
                                     0.4115450978279114,
                                     0.18498258292675018]}],
                        :uuid  "i&(#uW1}BkH+yp-bB(Px",
                        :vs    [5.0, 1.7999999523162842, 1.5],
                        :world [{:lon [-87.62229404185715, -87.62229624638798, -87.62225907814978, -87.62225435326927],
                                 :lat [41.8875858449021, 41.887648626494226, 41.887646880111674, 41.887574261705005]}]},
               :h      {:n    "NDM Parking Area 1",
                        :e    true,
                        :des  "",
                        :ch   1,
                        :uuid "3KB:P(&oP5G@!P=on@JH",
                        :tag  "",
                        :t    1496963502623000,
                        :u    "nr"}
               :nodeid "ubernode",
               :name   "NonDemarcatedParkingConfig"},
         cfg4    {:nodeid  "ubernode",
                 :name    "AllConfig",
                 #_( :prev    [{:cfg  {:roi {:n     "Parking_Area_1",
                                         :img   [{:v [0.34626907110214233,
                                                      0.15447795391082764,
                                                      0.18441490828990936,
                                                      0.35074955224990845],
                                                  :u [0.12549659609794617,
                                                      0.20654840767383575,
                                                      0.32884645462036133,
                                                      0.3006282448768616]}],
                                         :uuid  "Ei[({v+jvmW$hmCp?WfB",
                                         :vs    [5.0, 1.7999999523162842, 1.5],
                                         :spots [{:img   {:v [0.09406828135251999,
                                                              0.09406828135251999,
                                                              0.34365329146385193,
                                                              0.34365329146385193],
                                                          :u [0.18264101445674896,
                                                              0.29642608761787415,
                                                              0.29642608761787415,
                                                              0.18264101445674896]},
                                                  :e     true,
                                                  :uuid  "P>TtR]N6t6G2bWyle0bW",
                                                  :world {:lon [138.60647761850913, 138.60652918874038, 138.60653577727422, 138.60648420704297],
                                                          :lat [-34.924735570660815,
                                                                -34.92475064414831,
                                                                -34.92473535337679,
                                                                -34.92472027988875]}}],
                                         :world [{:lon [138.60647453422987, 138.6065403808459, 138.60653501614593, 138.60647686033457],
                                                  :lat [-34.92470793826165, -34.92472803160142, -34.92476101479756, -34.92474486341323]}]},
                                   :h   {:n    "Parking Area 1",
                                         :e    true,
                                         :des  "",
                                         :ch   0,
                                         :uuid "Gytxh]o-idKh$1SG0*JE",
                                         :tag  "",
                                         :t    1496964335136000,
                                         :u    "nr"}},
                            :type "dpark"},
                           {:cfg  {:roi {:n     "Traffic_2",
                                         :img   [{:v [0.26363807916641235, 0.24697142839431763, 0.680304765701294, 0.580304741859436],
                                                  :u [0.3318575918674469,
                                                      0.6834200620651245,
                                                      0.6427950859069824,
                                                      0.23654508590698242]}],
                                         :uuid  ">6fiI0VK>%Uva}KgF[?v",
                                         :world [{:lon [-87.62234864665669, -87.62234764492784, -87.62228974898218, -87.62228806258894],
                                                  :lat [41.887602378949275, 41.887687472689684, 41.8876873270469, 41.88758648175496]}]},
                                   :h   {:n    "Object.Enter",
                                         :e    true,
                                         :des  "",
                                         :ch   1,
                                         :uuid "4bqYyT6wD@X<S*PxF4^w",
                                         :tag  "",
                                         :t    1491874813254000,
                                         :u    "nr"}},
                            :type "objent"},
                           {:cfg  {:roi {:n     "Traffic_2",
                                         :img   [{:v [0.26363807916641235, 0.24697142839431763, 0.680304765701294, 0.580304741859436],
                                                  :u [0.3318575918674469,
                                                      0.6834200620651245,
                                                      0.6427950859069824,
                                                      0.23654508590698242]}],
                                         :uuid  ">6fiI0VK>%Uva}KgF[?v",
                                         :world [{:lon [-87.62234864665669, -87.62234764492784, -87.62228974898218, -87.62228806258894],
                                                  :lat [41.887602378949275, 41.887687472689684, 41.8876873270469, 41.88758648175496]}]},
                                   :h   {:n    "Object.Leave",
                                         :e    true,
                                         :des  "",
                                         :ch   1,
                                         :uuid "y4hvy84Yf+IFdh3GH*km",
                                         :tag  "",
                                         :t    1491874810836000,
                                         :u    "nr"}},
                            :type "objlev"},
                           {:cfg  {:roi {:n     "Traffic_5",
                                         :img   [{:v [0.6608603000640869, 0.735860288143158],
                                                  :u [0.09592008590698242, 0.21623258292675018]}],
                                         :uuid  "5ocP&q8}9RUKEOwS)$@>",
                                         :world [{:lon [-87.62225794506419, -87.62225913307962],
                                                  :lat [41.887519665301745, 41.88758676073655]}]},
                                   :h   {:n    "Pedestrian.Count",
                                         :e    true,
                                         :des  "",
                                         :ch   1,
                                         :uuid "31B@OO0lqPO8Y>[VR8oc",
                                         :tag  "",
                                         :t    1497390093980000,
                                         :u    "nr"}},
                            :type "linec"},
                           {:cfg  {:roi {:n     "Parking_Area_1",
                                         :img   [{:v [0.5479455590248108, 0.5868344902992249, 0.8423900008201599, 0.7423900365829468],
                                                  :u [0.23967008292675018,
                                                      0.45217007398605347,
                                                      0.4115450978279114,
                                                      0.18498258292675018]}],
                                         :uuid  "i&(#uW1}BkH+yp-bB(Px",
                                         :vs    [5.0, 1.7999999523162842, 1.5],
                                         :world [{:lon [-87.62229404185715, -87.62229624638798, -87.62225907814978, -87.62225435326927],
                                                  :lat [41.8875858449021, 41.887648626494226, 41.887646880111674, 41.887574261705005]}]},
                                   :h   {:n    "Parking Area 1",
                                         :e    true,
                                         :des  "",
                                         :ch   1,
                                         :uuid "3KB:P(&oP5G@!P=on@JH",
                                         :tag  "",
                                         :t    1496963502623000,
                                         :u    "nr"}},
                            :type "ndpark"}])
                 :cfgs [
                           {:cfg  {:roi {:n     "Traffic_2",
                                         :img   [{:v [0.26363807916641235, 0.24697142839431763, 0.680304765701294, 0.580304741859436],
                                                  :u [0.3318575918674469,
                                                      0.6834200620651245,
                                                      0.6427950859069824,
                                                      0.23654508590698242]}],
                                         :uuid  ">6fiI0VK>%Uva}KgF[?v",
                                         :world [{:lon [-87.62234864665669, -87.62234764492784, -87.62228974898218, -87.62228806258894],
                                                  :lat [41.887602378949275, 41.887687472689684, 41.8876873270469, 41.88758648175496]}]},
                                   :h   {:n    "Object.Enter",
                                         :e    true,
                                         :des  "",
                                         :ch   1,
                                         :uuid "4bqYyT6wD@X<S*PxF4^w",
                                         :tag  "",
                                         :t    1491874813254000,
                                         :u    "nr"}},
                            :type "objent"},
                           {:cfg  {:roi {:n     "Traffic_2",
                                         :img   [{:v [0.26363807916641235, 0.24697142839431763, 0.680304765701294, 0.580304741859436],
                                                  :u [0.3318575918674469,
                                                      0.6834200620651245,
                                                      0.6427950859069824,
                                                      0.23654508590698242]}],
                                         :uuid  ">6fiI0VK>%Uva}KgF[?v",
                                         :world [{:lon [-87.62234864665669, -87.62234764492784, -87.62228974898218, -87.62228806258894],
                                                  :lat [41.887602378949275, 41.887687472689684, 41.8876873270469, 41.88758648175496]}]},
                                   :h   {:n    "Object.Leave",
                                         :e    true,
                                         :des  "",
                                         :ch   1,
                                         :uuid "y4hvy84Yf+IFdh3GH*km",
                                         :tag  "",
                                         :t    1491874810836000,
                                         :u    "nr"}},
                            :type "objlev"}],
                 :orgid   "uberorg",
                 :siteid  "ubersite"}

         msg1 (msgpk/pack (stringify-keys cfg1))
         msg2 (msgpk/pack (stringify-keys ev1))
         msg3 (msgpk/pack (stringify-keys cfg2))
         msg4 (msgpk/pack (stringify-keys cfg3))
         msg5 (msgpk/pack (stringify-keys cfg4))
         logmsg (atom [])]

    (do
      (.start (Thread. (fn [] (worker/process-device-msg msg1)
      (worker/process-device-msg msg2)
      (worker/process-device-msg msg3)
      (worker/process-device-msg msg4)
      (worker/process-device-msg msg5)
                         )))
      (Thread/sleep 4000)
      ;(Thread/sleep 2000)
      (let [
             deleted-traffic-config (spy :debug (keywordize-keys (first (logger/get_traffic_config "ubersite" "ubernode" {:eventid "09631C54-9B94-4414-9BE3-9835B34B1C46"}  ))))
             deleted-parking-config (spy :debug (keywordize-keys (json/read-str (:config (spy :debug (keywordize-keys (first (logger/get_one_parkingzone "ubersite" "83EF9DFF-F398-4C34-8FC8-4C9182B58BB9" ))))))))
             parking_info (spy :debug (keywordize-keys (logger/get_latest_parking_info "ubersite")))]
        (is (not (:active deleted-traffic-config)))
        (is (not (:active deleted-parking-config)))
        ; (doseq [info parking_info
        ;         :when (= "83EF9DFF-F398-4C34-8FC8-4C9182B58BB9" (:parkingzoneid info))
        ;         :let [active (:active (spy :debug info))]]
        ;   (is (not active))
        ;   )
        ;(is (= 1 1))
        )

      )))


(deftest test-process-device-msg
  (let [a-second-ago (-> 1
                         time-core/seconds
                         time-core/ago
                         time-coerce/to-long
                         (* 1000))
        data {:name "SensorSample"
              :nodeid "ubernode"
              :sensor "l"
              :value 1000.0
              :time a-second-ago}
        msg (msgpk/pack (stringify-keys data))
        gps {:nodeid "ubernode"
             :name "GpsSample"
             :lat "37.3809866"
             :lon "-121.99231329"}
        logmsg (atom [])
        gpsmsg (msgpk/pack (stringify-keys gps))
        query "MATCH(n:Node {nodeid: \"ubernode\"}) RETURN {nodeid: n.nodeid, latitude_gps: n.latitude_gps, longitude_gps: n.longitude_gps} AS node"]

;    (logger/log_sensor_sample {:nodeid "ubernode" :sensor "l" :value 1000.0 :time 1})
;    (logger/log_sensor_sample {:nodeid "ubernode" :sensor "l" :value 1000.0 :time 1})
;    (logger/log_sensor_sample {:nodeid "ubernode" :sensor "l" :value 1000.0 :time 1})
;    (logger/log_sensor_sample {:nodeid "ubernode" :sensor "l" :value 1000.0 :time 1})
;    (logger/log_sensor_sample {:nodeid "ubernode" :sensor "p" :value 1 :time 1})
;    (logger/log_sensor_sample {:nodeid "ubernode" :sensor "lt" :value 10.0 :time 1})

    (with-redefs [ac/put-sensor-sample #(swap! logmsg conj %1)]
      (worker/process-device-msg msg)
      ; Change millilux to lux
      (is (= @logmsg [{:nodeid "ubernode", :sensor "l", :value 1000.0, :time a-second-ago}])))

    (worker/process-device-msg gpsmsg)

    (let [{:keys [nodeid latitude_gps longitude_gps]} (:node (keywordize-keys (json/read-str (neo4j/executeQuery query))))]
      (is (= (:nodeid gps) nodeid))
      (is (= (:lat gps) latitude_gps))
      (is (= (:lon gps) longitude_gps)))))

(deftest test-resolve-node
  (let [data {:values [18446744073703063493 18446744073703063493]
         :name "SensorSample"
         :sensor "rf"
         :units ""
         :time 1474315227256652
         :nodeid "N01334186"
         :value 18446744073703063493}
        result (try
                 (worker/resolve-node data)
                 true
                 (catch Exception e
                   false))]
    (is result)))

(deftest test-fix-units

  (let [data {:name "SensorSample"
              :nodeid "ubernode"
              :sensor "mt"
              :value 1000.0
              :time 1}
        msg (msgpk/pack (stringify-keys data))]

    (worker/resolve-units (keywordize-keys (msgpk/unpack msg)))
    ))

(deftest test-alarms

  (let [major {:name "DeviceAlarm"
              :nodeid "ubernode"
              :alarmType "HWFail_EEPROM"
              :alarmSeverity "Major"
              :category "InternalSensor"
              :msg ""}
        minor {:name "DeviceAlarm"
              :nodeid "ubernode"
              :alarmType "HWFail_EEPROM"
              :alarmSeverity "Minor"
              :category "InternalSensor"
              :msg ""}
        clear {:name "DeviceAlarm"
               :nodeid "ubernode"
               :alarmType "HWFail_EEPROM"
               :alarmSeverity "Clear"
               :category "InternalSensor"
               :msg ""}
        disconnect {:name "ConnectionStatus"
               :nodeid "ubernode"
               :status "disconnected"}
        connect {:name "ConnectionStatus"
                 :nodeid "ubernode"
                 :status "connected"}
        search {:user "device" :type "getAllAlerts" :siteprops {:siteid "ubersite"}}
        search_sys {:user "device" :type "getAlertSys" :alertprops {:alertid ""} }
        search_nnt {:user "device" :type "getAlertByNodeNameType" :alertprops {:nodeid "ubernode" :name "DeviceAlarm" :type "HWFail_EEPROM"} }
        line_cross_config ""
        cass-update (atom {})
        mqtt-update (atom {})
        kafka-update (atom {})
        process-device-msg (atom 0)
        disconnect-kafka {"name" "DeviceAlarm", "nodeid" "ubernode", "alarmType" "Disconnect", "alarmSeverity" "Critical", "msg" "Node Disconnected"}
        connect-kafka {"name" "DeviceAlarm","nodeid" "ubernode", "alarmType" "Disconnect", "alarmSeverity" "Clear", "msg" "Node Connected"}
        ;disconnect-cas {:nodeid "ubernode" :alarmtype "Disconnect", :severitycode "Minor", :message "Node Disconnected" :category "Network"}
        ;connect-cas {:nodeid "ubernode" :alarmtype "connected" :severitycode "Minor" :category "Network"}

        nodeid "ubernode"
        ;siteid "ubersite"
        ;orgid "uberorg"
        topic (format "/streamv1/faileddelivery/legacy/%s/ConfigRespUpdate" nodeid)
        msgmap {"name" "failed-delivery" "nodeid"  nodeid "msg" "failed delivery"}
        packed-map (msgpk/pack msgmap)
        mqtt-message (mqttlib/create-message-from-bytes packed-map)



        ]

    (defn get-cas-map [msgmap]
      (->
        msgmap
        (dissoc :name)
        (rename-keys {:alarmType :alarmtype
                      :status :alarmtype
                      :alarmSeverity :severitycode
                      :msg :message})))

    (defn get-mqtt-map [msgmap]
      (->
        msgmap
        (assoc :orgid "uberorg" :nodeid "ubernode" :siteid "ubersite")
        (rename-keys {:alarmType :type
                      :alarmSeverity :severity})))
    (defn get-mqtt-map-da [msgmap]
      (->
        msgmap
        (assoc :orgid "uberorg" :nodeid "ubernode" :siteid "ubersite" :nodehw "unode-v4" :siteaddress nil :sitename "Uber Site" :bssid nil :orgname "Uber Org" :nodename "Uber Node")
        (rename-keys {:alarmType :type
                      :alarmSeverity :severity})))

    (defn get-cas-msg []
      (dissoc @cass-update :alertid))

    (defn get-kafka-msg []
      (->
        (:msg @kafka-update)
        (json/read-str)))

    (defn get-mqtt-msg []
      (->
        (:msg @mqtt-update)
        (json/read-str :key-fn keyword)
        (dissoc :alertid)))

    (with-redefs [mqttcore/mqtt-publish #(swap! mqtt-update assoc :topic %1 :msg %2 :client-id %3)
                  logger/log_device_alarms #(swap! cass-update conj %1)
                  producer/kafka-alert #(swap! kafka-update assoc :msg %1)]

    (worker/process-device-msg (msgpk/pack (stringify-keys major)))
    (worker/process-device-msg (msgpk/pack (stringify-keys minor)))

    (let [result (cape/db-selector (cape/template-selector (cape/checkprops search)))
          items (get (json/read-str result) "items" [])
          array (if (map? items) [items] items)
          num_one (first array)
          alertid (get num_one "alertid")
          params_sys (assoc-in search_sys [:alertprops :alertid] alertid)
          result_sys (cape/db-selector (cape/template-selector (cape/checkprops params_sys)))
          num_one_sys (get (json/read-str result_sys) "alert")
          result_nnt (cape/db-selector (cape/template-selector (cape/checkprops search_nnt)))
          num_one_nnt (get (json/read-str result_nnt) "alert")]
      (is (= 1 (count array)))
      (is (= (get-cas-msg) (get-cas-map minor)))
      (is (= num_one num_one_sys))
      (is (= num_one num_one_nnt))
      )

    (let [result (cape/db-selector (cape/template-selector (cape/checkprops search)))
          items (get (json/read-str result) "items" [])
          array (if (map? items) [items] items)]
      (is (= 1 (count array)))
      (is (= (get-cas-msg) (get-cas-map minor)))
      (is (= (get-mqtt-msg) (get-mqtt-map-da minor)))
      )

    (worker/process-device-msg (msgpk/pack (stringify-keys clear)))

    (let [result (cape/db-selector (cape/template-selector (cape/checkprops search)))
          items (get (json/read-str result) "items" [])
          array (if (map? items) [items] items)]
      (is (= 0 (count array)))
      (is (= (get-cas-msg) (get-cas-map clear)))
      (is (= (get-mqtt-msg) (get-mqtt-map-da clear)))
      )

    ; Test disconnect
    (worker/process-device-msg (msgpk/pack (stringify-keys disconnect)))

    (is (= (get-kafka-msg) disconnect-kafka))
    (is (= (get-mqtt-msg) (get-mqtt-map disconnect)))

    ; Test connect
    (worker/process-device-msg (msgpk/pack (stringify-keys connect)))

    (is (= (get-kafka-msg) connect-kafka))
    (is (= (get-mqtt-msg) (get-mqtt-map connect)))

    ; Multiple disconnects
    (worker/process-device-msg (msgpk/pack (stringify-keys disconnect)))
    (worker/process-device-msg (msgpk/pack (stringify-keys disconnect)))

    (is (= (get-kafka-msg) disconnect-kafka))
    (is (= (get-mqtt-msg) (get-mqtt-map disconnect)))

    (with-redefs [worker/process-device-msg #(swap! process-device-msg inc)]
      (listener/failed-delivery topic mqtt-message)
      (Thread/sleep 200)
      (is (= 0 @process-device-msg)))

    ; Multiple connects
    (worker/process-device-msg (msgpk/pack (stringify-keys connect)))
    (worker/process-device-msg (msgpk/pack (stringify-keys connect)))

    (is (= (get-kafka-msg) connect-kafka))
    (is (= (get-mqtt-msg) (get-mqtt-map connect)))

    ; Handle node status update when node is disconnected via MQTT /streamv1/faileddelivery/legacy/
    ;(worker/mqtt-send-message orgid siteid nodeid topic msgmap)

      (let [original worker/process-device-msg
            p (promise)]
        (with-redefs [worker/process-device-msg (fn [& args]
                                                  (apply original args)
                                                  (deliver p nil))]
          (listener/failed-delivery topic mqtt-message)
          @p))

      (is (= (get-kafka-msg) disconnect-kafka))
      (is (= (get-mqtt-msg) (get-mqtt-map disconnect))))))

#_(deftest time-traveling-sensor-samples
  (let [a-year-and-a-day-ago (-> 1
                                 time-core/years
                                 time-core/ago
                                 (time-core/minus (time-core/days 1))
                                 time-coerce/to-long
                                 (* 1000))
        lightsample (msgpk/pack (stringify-keys {:name "SensorSample"
                                                 :nodeid "NS1"
                                                 :sensor "lt"
                                                 :value 3.1415
                                                 :time 54321}))
        hss worker/handle-sensor-sample
        confirm-handle-sensor-sample (promise)
        mf worker/metric-factory
        hg histograms/histogram
        my-hg-args (promise)
        my-hg (promise)
        update histograms/update!
        updated (promise)]
    (with-redefs [worker/handle-sensor-sample (fn [& args]
                                                (deliver confirm-handle-sensor-sample args)
                                                (apply hss args))
                  worker/metric-factory (fn [& [type _ :as args]]
                                          (if (= :histogram
                                                 type)
                                            (do (deliver my-hg-args args)
                                                (deliver my-hg
                                                         (apply mf args))
                                                @my-hg)
                                            (apply mf args)))
                  histograms/histogram (fn [& args]
                                         (deliver my-hg-args args)
                                         (deliver my-hg
                                                  (apply hg args))
                                         @my-hg)
                  histograms/update! (fn [& args]
                                       (deliver updated args)
                                       (apply update args))]
      (worker/process-device-msg lightsample))
    (is (false? (realized? confirm-handle-sensor-sample)))
    (is (= [:histogram ["timetraveler" "NS1" "lt"]] (deref my-hg-args
                                                           5000
                                                           nil)))
    (is (and (some? (deref my-hg
                      5000
                      nil))
             (instance? com.codahale.metrics.Histogram
                        (deref my-hg
                          5000
                          nil))))
    (is (= [(deref my-hg
              5000
              nil) 54321]
           (deref updated
                  5000
                  nil)))))

(deftest backfill-gps-coordinates-test
  (let [;; Here I address a bug where the `testdata.cypher` file has
        ;; two root-most Orgs, not one, where code in ACL expects only
        ;; one. The former is for intended use by the sensity admin
        ;; user, where the latter is intended to be used by
        ;; `uberuser`. This hack is necessary so `assignUser` will run
        ;; without shortcircuiting the ACL logic.
        po "uberorg"]
    (alter-var-root #'acl.core/get-sensity-org-id
                    (constantly (delay po)))

    ;; Simulate David's proposed setup. Verify for a number of Nodes
    ;; (no lat/lon, partial lat/lon, emptied lat/lon), that after
    ;; commissioning, the first read contains no lat/lon data. Then
    ;; verify after a `GpsSample` event the Nodes report the expect
    ;; lat/lon.
    (let [;; Given a set of Nodes created with `createEmptyNode`
          nodes (->> [{}
                      {:latitude "3.1415"}
                      {:latitude ""
                       :longitude ""}
                      {:latitude nil
                       :longitude nil}]
                     (map (juxt create-test-node-empty identity))
                     (into {}))
          ;; And a Site to assign them to
          orgid (create-test-org)
          siteid (create-test-site orgid)
          gps-sample {:name "GpsSample"
                      :lat "37.3809866"
                      :lon "-121.99231329"}]
      (doseq [[nodeid {:keys [latitude longitude]
                       :as coordinates}]
              nodes]
        ;; First, backfill questionable coordinates
        (neo4j/executeQuery "
MATCH (n:Node)
WHERE n.nodeid = {props}.nodeid
SET n.latitude = {props}.latitude
SET n.longitude = {props}.longitude
"
                            {"nodeid" nodeid
                             "latitude" latitude
                             "longitude" longitude})
        (testing "Site-less Nodes do not update lat/lon"
          (let [gps-sample (assoc gps-sample
                                  :nodeid nodeid)]
            (-> gps-sample
                stringify-keys
                msgpk/pack
                worker/process-device-msg)
            (let [{:keys [success
                          node]} (with-redefs [acl.core/actrl identity]
                                   (run {:type "getNode"
                                         :user *user*
                                         :nodeprops {:nodeid nodeid}}))]
              (is (= true
                     success))
              (is (= latitude
                     (:latitude node)))
              (is (= longitude
                     (:longitude node))))))
        ;; Cannot `getNode` on a Node without a Site.
        (testing "assigning Node to Site"
          (is (= {:success true
                  :node {:nodeid nodeid}}
                 (run {:type "assignNode"
                       :user *user*
                       :orgprops {:orgid orgid}
                       :siteprops {:siteid siteid}
                       :nodeprops {:nodeid nodeid}}))))
        (testing "Node has original lat/lon"
          (let [{:keys [success
                        node]} (run {:type "getNode"
                                     :user *user*
                                     :nodeprops {:nodeid nodeid}})]
            (is (= true
                   success))
            (is (= latitude
                   (:latitude node)))
            (is (= longitude
                   (:longitude node)))))
        (let [gps-sample (assoc gps-sample
                                :nodeid nodeid)
              gps-sample-2 (assoc gps-sample
                                  :lat "40.0"
                                  :lon "-110.0")]
          ;; After a `GpsSample`
          (-> gps-sample
              stringify-keys
              msgpk/pack
              worker/process-device-msg)
          ;; Show Nodes now use that data for their lat/lon data.
          (let [{:keys [success
                        node]} (run {:type "getNode"
                                     :user *user*
                                     :nodeprops {:nodeid nodeid}})
                {:keys [lat
                        lon]} gps-sample
                coords {:latitude lat
                        :longitude lon
                        :latitude_gps lat
                        :longitude_gps lon}]
            (is (= true
                   success))
            (is (= (select-keys node (keys coords))
                   coords)))
          ;; After user-facing lat/lon is set, another `GpsSample`
          (-> gps-sample-2
              stringify-keys
              msgpk/pack
              worker/process-device-msg)
          ;; Show that nodes retain their first lat/lon data, but
          ;; update their GPS data.
          (let [{:keys [success
                        node]} (run {:type "getNode"
                                     :user *user*
                                     :nodeprops {:nodeid nodeid}})
                {:keys [lat
                        lon]} gps-sample
                {latg :lat
                 long :lon} gps-sample-2
                coords {:latitude lat
                        :longitude lon
                        :latitude_gps latg
                        :longitude_gps long}]
            (is (= true
                   success))
            (is (= (select-keys node (keys coords))
                   coords))))))))
