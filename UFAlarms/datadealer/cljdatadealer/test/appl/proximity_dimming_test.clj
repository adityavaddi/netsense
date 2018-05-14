(ns appl.proximity-dimming-test
  (:require [appl.lighting-control.utils :as light-utils]
            [appl.lighting-control.utils-test :as light-utils-test]
            [appl.proximity-dimming :as proximity-dimming]
            [clj-time
             [coerce :as clj-time-coerce]
             [core :as clj-time]]
            [clojure.core.async :as async]
            [clojure.test :refer :all]
            [clojure.tools.logging :refer :all]
            [dealer
             [devsvcctrl :as dev-ctrl]
             [devsvcctrl-test :as dev-ctrl-test]]
            [logging.activitylogger :as actlog]
            [mqtt.fixture :as mqtt]
            [neowrap.neowrapper :as neo4j]
            [utils
             [cape-test :refer [vectors-to-sets
                                uuid
                                run
                                *user*
                                create-test-org
                                default-test-site
                                create-test-site
                                create-test-node
                                create-test-lighting-group
                                default-test-pdprofile
                                create-test-pdprofile]]
             [cassandra_fixture :as cass]
             [neo4j-fixture :as neo4j-fixture]]
            [utils.async.async_chans :as ac]
            [kafka.mock :as kafka]))

(use-fixtures :once (join-fixtures [cass/cassandra-fixture
                                    neo4j-fixture/neo4j-fixture
                                    mqtt/mqtt-fixture
                                    kafka/kafka-fixture
                                    dev-ctrl-test/device-service-control-fixture
                                    utils.cape-test/mock-google-api]))

(def joda-time-to-node-time
  (comp long
        #(* 1e3 %)
        clj-time-coerce/to-long))

(deftest node-time-to-joda-test
  (let [t (clj-time/now)
        node-time (joda-time-to-node-time t)]
    (is (= t
           (proximity-dimming/node-time-to-joda node-time)))))

(deftest update-motion-map-test
  (with-redefs [proximity-dimming/presence-values (atom {})]
    (testing "with a no-motion value"
      (let [t (joda-time-to-node-time (clj-time/now))]
        (is (= {:n {:time (proximity-dimming/node-time-to-joda t)
                    :value 1.0}}
               (proximity-dimming/handle-presence-value {:time t
                                                         :value 1.0
                                                         :nodeid :n})
               @proximity-dimming/presence-values))))
    (testing "with a motion value"
      (let [t (joda-time-to-node-time (clj-time/now))]
        (is (= {:n {:time (proximity-dimming/node-time-to-joda t)
                    :value 1.0}}
               (proximity-dimming/handle-presence-value {:time t
                                                         :value 1.0
                                                         :nodeid :n})
               @proximity-dimming/presence-values))))))

(deftest before-time-test
  (let [t1 (clj-time/now)
        t2 (clj-time/minus t1 (clj-time/seconds 1))]
    (is (true? ((proximity-dimming/off-and-for-less-than t1)
                [:nodeid {:time t2 :value 0.0}])))
    (is (true? ((proximity-dimming/off-and-for-less-than t1)
                [:nodeid {:time t1 :value 0.0}])))
    (is (false? ((proximity-dimming/off-and-for-less-than t2)
                 [:nodeid {:time t1 :value 0.0}])))
    (is (false? ((proximity-dimming/off-and-for-less-than t1)
                 [:nodeid {:time t1 :value 1.0}])))))

(deftest active-nodes-test
  (let [[t1 t2 t3 t4 t5
         :as times] (->> (clj-time/now)
                         (iterate #(clj-time/plus % (clj-time/seconds 1)))
                         (map joda-time-to-node-time))
        nodes (map (fn [time name]
                     {:time time
                      :nodeid name
                      :value 1.0})
                   [t1 t2 t3 t4 t5]
                   [:1 :2 :3 :4 :5])]
    (with-redefs [proximity-dimming/presence-values (atom {})]
      (dorun
       (map proximity-dimming/handle-presence-value
            nodes))
      (is (= (filter (comp #{:1 :2 :3 :4 :5}
                           :nodeid)
                     nodes)
             (proximity-dimming/recently-active-nodes @proximity-dimming/presence-values
                                                      nodes
                                                      (proximity-dimming/node-time-to-joda t3)))))))

(deftest get-neighbors-test
  (testing "With two nodes two meters apart"
    (let [node-a {:nodeid "node-a"
                  :lat "37.0"
                  :lon "-121.0"}
          node-b (-> node-a
                     (assoc :nodeid "node-b")
                     (update :lon str "00018"))]
      (testing "see that it's within 2 meters, but not 1 meter"
        (is (= []
               (proximity-dimming/get-neighbors [node-a] 1 node-b)))
        (is (= [(proximity-dimming/parse-lat-lon node-a)]
               (proximity-dimming/get-neighbors [node-a] 2 node-b))))
      (testing "verify motion-detector mode"
        (is (= []
               (proximity-dimming/get-neighbors [node-a] 0 node-b))))
      (testing "verify no-radius mode"
        (is (= [node-a]
               (proximity-dimming/get-neighbors [node-a] nil node-b)))))))

(defn strip-node-lat-lon
  [nodeid]
  (neo4j/executeQuery (str "match (n:Node {nodeid: \"" nodeid "\"}) remove n.latitude remove n.longitude return n")))

(deftest single-node-test
  (testing "Simplest, straightforward case"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          nodeid (create-test-node orgid siteid)

          minLevel 23
          maxLevel 79

          pdprofileid (create-test-pdprofile orgid siteid)

          apply-pd {:type "applyPDtoNodes"
                    :user "uberuser"
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :pdprofileprops {:pdprofileid pdprofileid}
                    :nodeprops {:nodeids [nodeid]}}

          time (atom 0)
          send-sensor-sample (fn [value]
                               (ac/put-sensor-sample
                                {:nodeid nodeid
                                 :sensor "p"
                                 :value value
                                 :time (* 1e6
                                          (swap! time inc))})
                               (-> pdprofileid
                                   proximity-dimming/get-pdprofile
                                   proximity-dimming/run-controller))

          max-level [[#{} minLevel] [#{nodeid} maxLevel]]
          min-level [[#{nodeid} minLevel] [#{} maxLevel]]]

      (is (true? (-> apply-pd
                     run
                     :success)))

      (with-redefs [proximity-dimming/light #(do %&)
                    proximity-dimming/presence-values (atom {})
                    ac/put-sensor-sample proximity-dimming/handle-presence-value
                    light-utils/get-now #(-> time
                                             deref
                                             (* 1e6)
                                             proximity-dimming/node-time-to-joda)]
        (testing "should go to max-level when presence *is* detected"
          (is (= max-level
                 (send-sensor-sample 1.0)))
          (is (= max-level
                 (send-sensor-sample 1.0))))
        (testing "should only go off once enough time has passed"
          (is (= max-level
                 (send-sensor-sample 0.0)))
          (is (= max-level
                 (send-sensor-sample 0.0)))
          (is (= min-level
                 (send-sensor-sample 0.0)))))))

  (testing "Node, somehow with lat/lon removed"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          nodeid (create-test-node orgid siteid)

          minLevel 23
          maxLevel 79

          pdprofileid (create-test-pdprofile orgid siteid)

          apply-pd {:type "applyPDtoNodes"
                    :user "uberuser"
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :pdprofileprops {:pdprofileid pdprofileid}
                    :nodeprops {:nodeids [nodeid]}}

          time (atom 0)
          send-sensor-sample (fn [value]
                               (ac/put-sensor-sample
                                {:nodeid nodeid
                                 :sensor "p"
                                 :value value
                                 :time (* 1e6
                                          (swap! time inc))})
                               (-> pdprofileid
                                   proximity-dimming/get-pdprofile
                                   proximity-dimming/run-controller))

          max-level [[#{} minLevel] [#{nodeid} maxLevel]]
          min-level [[#{nodeid} minLevel] [#{} maxLevel]]]

      (is (true? (-> apply-pd
                     run
                     :success)))

      (strip-node-lat-lon nodeid)

      (with-redefs [proximity-dimming/light #(do %&)
                    proximity-dimming/presence-values (atom {})
                    ac/put-sensor-sample proximity-dimming/handle-presence-value
                    light-utils/get-now #(-> time
                                             deref
                                             (* 1e6)
                                             proximity-dimming/node-time-to-joda)]
        (testing "should go to max-level when presence *is* detected"
          (is (= max-level
                 (send-sensor-sample 1.0)))
          (is (= max-level
                 (send-sensor-sample 1.0))))
        (testing "should only go off once enough time has passed"
          (is (= max-level
                 (send-sensor-sample 0.0)))
          (swap! time inc)
          (is (= max-level
                 (-> pdprofileid
                     proximity-dimming/get-pdprofile
                     proximity-dimming/run-controller)))
          (swap! time inc)
          (is (= min-level (-> pdprofileid
                               proximity-dimming/get-pdprofile
                               proximity-dimming/run-controller))))))))

(deftest two-node-test
  (testing "two nodes, within eachother's circle of influence"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          node-a (create-test-node orgid siteid)
          node-b (create-test-node orgid siteid)

          nodeList [node-a node-b]

          groupid (create-test-lighting-group orgid
                                              siteid
                                              {:nodeList nodeList})

          minLevel 23
          maxLevel 79

          pdprofileid (create-test-pdprofile orgid siteid)

          apply-pd {:type "applyPDtoGroup"
                    :user "uberuser"
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :pdprofileprops {:pdprofileid pdprofileid}
                    :groupprops {:groupids [groupid]}}


          time (atom 0)
          send-sensor-sample (fn [value]
                               (ac/put-sensor-sample
                                {:nodeid node-a
                                 :sensor "p"
                                 :value value
                                 :time (* 1e6
                                          (swap! time inc))})
                               (-> pdprofileid
                                   proximity-dimming/get-pdprofile
                                   proximity-dimming/run-controller))

          max-level [[#{} minLevel]
                     [#{node-a node-b} maxLevel]]
          min-level [[#{node-a node-b} minLevel]
                     [#{} maxLevel]]]

      (is (= (-> {:result (assoc default-test-pdprofile
                                 :pdprofileid pdprofileid
                                 :nodes (mapv #(do {:nodeid %})
                                              nodeList)
                                 :groups [{:groupid groupid
                                           :name "Test Lighting Group"}]
                                 :sites [])
                  :success true}
                 vectors-to-sets)
             (-> apply-pd
                 run
                 vectors-to-sets)))

      (with-redefs [proximity-dimming/light #(do %&)
                    proximity-dimming/presence-values (atom {})
                    ac/put-sensor-sample proximity-dimming/handle-presence-value
                    light-utils/get-now #(-> time
                                             deref
                                             (* 1e6)
                                             proximity-dimming/node-time-to-joda)]
        (testing "should go to max-level when presence *is* detected"
          (is (= max-level
                 (send-sensor-sample 1.0)))
          (is (= max-level
                 (send-sensor-sample 1.0))))
        (testing "should only go off once enough time has passed"
          (is (= max-level
                 (send-sensor-sample 0.0)))
          (swap! time inc)
          (is (= max-level
                 (-> pdprofileid
                     proximity-dimming/get-pdprofile
                     proximity-dimming/run-controller)))
          (swap! time inc)
          (is (= min-level (-> pdprofileid
                               proximity-dimming/get-pdprofile
                               proximity-dimming/run-controller)))))))
  (testing "Two nodes, outside of eachother's radius (motion detection mode)"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          node-a (create-test-node orgid siteid)
          node-b (create-test-node orgid siteid)
          nodeList [node-a node-b]

          groupid (create-test-lighting-group orgid
                                              siteid
                                              {:nodeList nodeList})

          minLevel 23
          maxLevel 79

          pdprofileid (create-test-pdprofile orgid siteid {:radius 0 :mode "radius"})

          apply-pd {:type "applyPDtoGroup"
                    :user "uberuser"
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :pdprofileprops {:pdprofileid pdprofileid}
                    :groupprops {:groupids [groupid]}}


          time (atom 0)
          send-sensor-sample (fn [node value]
                               (ac/put-sensor-sample
                                {:nodeid node
                                 :sensor "p"
                                 :value value
                                 :time (* 1e6
                                          (swap! time inc))})
                               (-> pdprofileid
                                   proximity-dimming/get-pdprofile
                                   proximity-dimming/run-controller))]

      (is (true? (-> apply-pd
                     run
                     :success)))

      (dorun (map strip-node-lat-lon nodeList))

      (with-redefs [proximity-dimming/light #(vec %&)
                    proximity-dimming/presence-values (atom {})
                    ac/put-sensor-sample proximity-dimming/handle-presence-value
                    light-utils/get-now #(-> time
                                             deref
                                             (* 1e6)
                                             proximity-dimming/node-time-to-joda)]
        (testing "both nodes toggle state individually"
          (testing "for node a"
            (is (= [[#{node-b} minLevel]
                    [#{node-a} maxLevel]]
                   (send-sensor-sample node-a 1.0)))
            (is (= [[#{node-b} minLevel]
                    [#{node-a} maxLevel]]
                   (send-sensor-sample node-a 0.0)))
            (swap! time inc)
            (swap! time inc)
            (is (= [[#{node-a node-b} minLevel]
                    [#{} maxLevel]]
                   (-> pdprofileid
                       proximity-dimming/get-pdprofile
                       proximity-dimming/run-controller))))
          (testing "for node b"
            (is (= [[#{node-a} minLevel]
                    [#{node-b} maxLevel]]
                   (send-sensor-sample node-b 1.0)))
            (is (= [[#{node-a} minLevel]
                    [#{node-b} maxLevel]]
                   (send-sensor-sample node-b 0.0)))
            (swap! time inc)
            (swap! time inc)
            (is (= [[#{node-a node-b} minLevel]
                    [#{} maxLevel]]
                   (-> pdprofileid
                       proximity-dimming/get-pdprofile
                       proximity-dimming/run-controller)))))))))

(deftest three-node-test
  (testing "Two motion detectors, one unaffected light (no lat/lon necessary)"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)

          [node-a node-b node-c
           :as nodeList] (repeatedly 3 #(create-test-node orgid siteid))

          groupid (create-test-lighting-group orgid
                                              siteid
                                              {:nodeList nodeList})

          minLevel 23
          maxLevel 79

          pdprofileid (create-test-pdprofile orgid siteid {:radius 0 :mode "radius"})

          apply-pd {:type "applyPDtoGroup"
                    :user "uberuser"
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :pdprofileprops {:pdprofileid pdprofileid}
                    :groupprops {:groupids [groupid]}}


          time (atom 0)
          send-sensor-sample (fn [node value]
                               (ac/put-sensor-sample
                                {:nodeid node
                                 :sensor "p"
                                 :value value
                                 :time (* 1e6
                                          (swap! time inc))})
                               (-> pdprofileid
                                   proximity-dimming/get-pdprofile
                                   proximity-dimming/run-controller))]

      (is (true? (-> apply-pd
                     run
                     :success)))

      (dorun (map strip-node-lat-lon nodeList))

      (with-redefs [proximity-dimming/light #(vec %&)
                    proximity-dimming/presence-values (atom {})
                    ac/put-sensor-sample proximity-dimming/handle-presence-value
                    light-utils/get-now #(-> time
                                             deref
                                             (* 1e6)
                                             proximity-dimming/node-time-to-joda)]
        (testing "both nodes toggle state individually"
          (testing "for node a"
            (is (= [[#{node-b node-c} minLevel]
                    [#{node-a} maxLevel]]
                   (send-sensor-sample node-a 1.0)))
            (is (= [[#{node-b node-c} minLevel]
                    [#{node-a} maxLevel]]
                   (send-sensor-sample node-a 0.0)))
            (swap! time inc)
            (swap! time inc)
            (is (= [[#{node-a node-b node-c} minLevel]
                    [#{} maxLevel]]
                   (-> pdprofileid
                       proximity-dimming/get-pdprofile
                       proximity-dimming/run-controller))))
          (testing "for node b"
            (is (= [[#{node-a node-c} minLevel]
                    [#{node-b} maxLevel]]
                   (send-sensor-sample node-b 1.0)))
            (is (= [[#{node-a node-c} minLevel]
                    [#{node-b} maxLevel]]
                   (send-sensor-sample node-b 0.0)))
            (swap! time inc)
            (swap! time inc)
            (is (= [[#{node-a node-b node-c} minLevel]
                    [#{} maxLevel]]
                   (-> pdprofileid
                       proximity-dimming/get-pdprofile
                       proximity-dimming/run-controller))))))))
  (testing "Two motion detectors, one affected light in no-radius mode (no lat/lon necessary)"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)

          [node-a node-b node-c
           :as nodeList] (repeatedly 3 #(create-test-node orgid siteid))

          groupid (create-test-lighting-group orgid
                                              siteid
                                              {:nodeList nodeList})

          minLevel 23
          maxLevel 79

          pdprofileid (create-test-pdprofile orgid siteid)

          apply-pd {:type "applyPDtoGroup"
                    :user "uberuser"
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :pdprofileprops {:pdprofileid pdprofileid}
                    :groupprops {:groupids [groupid]}}


          time (atom 0)
          send-sensor-sample (fn [node value]
                               (ac/put-sensor-sample
                                {:nodeid node
                                 :sensor "p"
                                 :value value
                                 :time (* 1e6
                                          (swap! time inc))})
                               (-> pdprofileid
                                   proximity-dimming/get-pdprofile
                                   proximity-dimming/run-controller))

          max-level [[#{} minLevel]
                     [(set nodeList) maxLevel]]
          min-level [[(set nodeList) minLevel]
                     [#{} maxLevel]]]

      (is (true? (-> apply-pd
                     run
                     :success)))

      (dorun (map strip-node-lat-lon nodeList))

      (with-redefs [proximity-dimming/light #(do %&)
                    proximity-dimming/presence-values (atom {})
                    ac/put-sensor-sample proximity-dimming/handle-presence-value
                    light-utils/get-now #(-> time
                                             deref
                                             (* 1e6)
                                             proximity-dimming/node-time-to-joda)]
        (testing "When node-a reports sensor values"
          (testing "should go to max-level when presence *is* detected"
            (is (= max-level
                   (send-sensor-sample node-a 1.0)))
            (is (= max-level
                   (send-sensor-sample node-a 1.0))))
          (testing "should only go off once enough time has passed"
            (is (= max-level
                   (send-sensor-sample node-a 0.0)))
            (swap! time inc)
            (is (= max-level
                   (-> pdprofileid
                       proximity-dimming/get-pdprofile
                       proximity-dimming/run-controller)))
            (swap! time inc)
            (is (= min-level
                   (-> pdprofileid
                       proximity-dimming/get-pdprofile
                       proximity-dimming/run-controller)))))
        (testing "When node-b reports sensor values"
          (testing "should go to max-level when presence *is* detected"
            (is (= max-level
                   (send-sensor-sample node-b 1.0)))
            (is (= max-level
                   (send-sensor-sample node-b 1.0))))
          (testing "should only go off once enough time has passed"
            (is (= max-level
                   (send-sensor-sample node-b 0.0)))
            (swap! time inc)
            (is (= max-level
                   (-> pdprofileid
                       proximity-dimming/get-pdprofile
                       proximity-dimming/run-controller)))
            (swap! time inc)
            (is (= min-level
                   (-> pdprofileid
                       proximity-dimming/get-pdprofile
                       proximity-dimming/run-controller))))))))
  (testing "Two motion detectors, one affected light in radius mode"
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)

          [node-a node-b node-c
           :as nodeList] (->> (range 3)
                              (map (comp #(create-test-node orgid siteid %)
                                         #(assoc {:latitude "37.0"}
                                                 :longitude %)
                                         #(format "%.6f" %)
                                         #(* %
                                             9e-6))))

          groupid (create-test-lighting-group orgid
                                              siteid
                                              {:nodeList nodeList})

          minLevel 23
          maxLevel 79

          pdprofileid (create-test-pdprofile orgid siteid {:radius 1 :mode "radius"})

          apply-pd {:type "applyPDtoGroup"
                    :user "uberuser"
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :pdprofileprops {:pdprofileid pdprofileid}
                    :groupprops {:groupids [groupid]}}


          time (atom 0)
          send-sensor-sample (fn [node value]
                               (ac/put-sensor-sample
                                {:nodeid node
                                 :sensor "p"
                                 :value value
                                 :time (* 1e6
                                          (swap! time inc))})
                               (-> pdprofileid
                                   proximity-dimming/get-pdprofile
                                   proximity-dimming/run-controller))

          max-level []
          min-level []]

      (is (true? (-> apply-pd
                     run
                     :success)))

      (with-redefs [proximity-dimming/light #(do %&)
                    proximity-dimming/presence-values (atom {})
                    ac/put-sensor-sample proximity-dimming/handle-presence-value
                    light-utils/get-now #(-> time
                                             deref
                                             (* 1e6)
                                             proximity-dimming/node-time-to-joda)]
        (testing "when node-a reports sensor values, node-b is affected"
          (testing "should go to max-level when presence *is* detected"
            (is (= [[#{node-c} minLevel]
                    [#{node-a node-b} maxLevel]]
                   (send-sensor-sample node-a 1.0)))
            (send-sensor-sample node-a 0.0)
            (send-sensor-sample node-b 0.0)
            (send-sensor-sample node-c 0.0)
            (swap! time inc)
            (swap! time inc)
            (is (= [[#{node-a node-b node-c} minLevel]
                    [#{} maxLevel]]
                   (-> pdprofileid
                       proximity-dimming/get-pdprofile
                       proximity-dimming/run-controller)))))
        (testing "when node-c reports sensor values, node-b is affected"
          (testing "should go to max-level when presence *is* detected"
            (is (= [[#{node-a} minLevel]
                    [#{node-b node-c} maxLevel]]
                   (send-sensor-sample node-c 1.0)))
            (send-sensor-sample node-a 0.0)
            (send-sensor-sample node-b 0.0)
            (send-sensor-sample node-c 0.0)
            (swap! time inc)
            (swap! time inc)
            (is (= [[#{node-a node-b node-c} minLevel]
                    [#{} maxLevel]]
                   (-> pdprofileid
                       proximity-dimming/get-pdprofile
                       proximity-dimming/run-controller)))))
        (testing "When node-a, and then node-c report sensor values, node-b only goes to min-level when both node-a and node-c go to min-level"
          (is (= [[#{node-c} minLevel]
                  [#{node-a node-b} maxLevel]]
                 (send-sensor-sample node-a 1.0)))
          (is (= [[#{} minLevel]
                  [#{node-a node-b node-c} maxLevel]]
                 (send-sensor-sample node-c 1.0)))
          (is (= [[#{} minLevel]
                  [#{node-a node-b node-c} maxLevel]]
                 (send-sensor-sample node-a 0.0)))
          (is (= [[#{} minLevel]
                  [#{node-a node-b node-c} maxLevel]]
                 (send-sensor-sample node-c 0.0)))
          (swap! time inc)
          (swap! time inc)
          (is (= [[#{node-a node-b node-c} minLevel]
                  [#{} maxLevel]]
                 (-> pdprofileid
                     proximity-dimming/get-pdprofile
                     proximity-dimming/run-controller))))))))

(deftest missing-lat-lon-test
  (let [orgid (create-test-org)
        siteid (create-test-site orgid)

        [node-a node-b
         :as nodeList] (->> (range 2)
                            (map (comp #(create-test-node orgid siteid %)
                                       #(assoc {:latitude "37.0"}
                                               :longitude %)
                                       #(format "%.6f" %)
                                       #(* %
                                           9e-6))))

        groupid (create-test-lighting-group orgid
                                            siteid
                                            {:nodeList nodeList})

        minLevel 23
        maxLevel 79

        pdprofileid (create-test-pdprofile orgid siteid {:radius 1 :mode "radius"})

        apply-pd {:type "applyPDtoGroup"
                  :user "uberuser"
                  :orgprops {:orgid orgid}
                  :siteprops {:siteid siteid}
                  :pdprofileprops {:pdprofileid pdprofileid}
                  :groupprops {:groupids [groupid]}}


        time (atom 0)
        send-sensor-sample (fn [node value]
                             (ac/put-sensor-sample
                              {:nodeid node
                               :sensor "p"
                               :value value
                               :time (* 1e6
                                        (swap! time inc))})
                             (-> pdprofileid
                                 proximity-dimming/get-pdprofile
                                 proximity-dimming/run-controller))]

    (is (= (-> {:result (assoc default-test-pdprofile
                               :radius 1
                               :mode "radius"
                               :pdprofileid pdprofileid
                               :nodes (mapv #(do {:nodeid %})
                                            nodeList)
                               :groups [{:groupid groupid
                                         :name "Test Lighting Group"}]
                               :sites [])
                :success true}
               vectors-to-sets)
           (-> apply-pd
               run
               vectors-to-sets)))

    (strip-node-lat-lon node-a)

    (with-redefs [proximity-dimming/light #(do %&)
                  proximity-dimming/presence-values (atom {})
                  ac/put-sensor-sample proximity-dimming/handle-presence-value
                  light-utils/get-now #(-> time
                                           deref
                                           (* 1e6)
                                           proximity-dimming/node-time-to-joda)]
      (testing "when node-a reports sensor values, other nodes don't care"
        (is (= [[#{node-b} minLevel]
                [#{node-a} maxLevel]]
               (send-sensor-sample node-a 1.0)))
        (send-sensor-sample node-a 0.0)
        (send-sensor-sample node-b 0.0)
        (swap! time inc)
        (swap! time inc)
        (is (= [[#{node-a node-b} minLevel]
                [#{} maxLevel]]
               (-> pdprofileid
                   proximity-dimming/get-pdprofile
                   proximity-dimming/run-controller))))
      (testing "when node-b reports sensor values, node-a is unaffected"
        (is (= [[#{node-a} minLevel]
                [#{node-b} maxLevel]]
               (send-sensor-sample node-b 1.0)))
        (send-sensor-sample node-a 0.0)
        (send-sensor-sample node-b 0.0)
        (swap! time inc)
        (swap! time inc)
        (is (= [[#{node-a node-b} minLevel]
                [#{} maxLevel]]
               (-> pdprofileid
                   proximity-dimming/get-pdprofile
                   proximity-dimming/run-controller)))))))

(deftest deduplicate-lfs-test
  ;; A feature requirement for PD is to avoid, if possible, sending a
  ;; LFS command to a Node if the Node is already at the desire light
  ;; level. This is done by keeping a local cache, `dim-values`, and
  ;; populating it with `lt` samples.

  ;; However, as `lt` samples only happen when there is a *change*, we
  ;; also backfill values from Cassandra, in case the Node is already
  ;; at the correct level. Further, if a sensor sample is not in
  ;; Cassandra, we log right before PD is about to send -- if the Node
  ;; is at the wrong level, a `lt` sample will update
  ;; `dim-values`. Otherwise, `dim-values` will have the PD
  ;; about-to-send value to prevent future duplicate LFS commands to
  ;; that Node.
  (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;")
  (let [fake-time (atom 0)
        orgid (create-test-org)
        siteid (create-test-site orgid)
        [node-at-minLevel-not-in-cassandra
         node-at-maxLevel-not-in-cassandra
         node-at-minLevel-in-cassandra
         node-at-maxLevel-in-cassandra
         :as nodeList] (repeatedly 4 #(create-test-node orgid siteid))
        lg (create-test-lighting-group orgid
                                       siteid
                                       {:nodeList nodeList})
        pdprofileid (create-test-pdprofile orgid siteid)
        apply-pdprofile {:type "applyPDtoGroup"
                         :user "uberuser"
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :pdprofileprops {:pdprofileid pdprofileid}
                         :groupprops {:groupids [lg]}}
        {:keys [minLevel
                maxLevel]} default-test-pdprofile
        log-light-map {:isscheduled false
                       :harvest_trigger false
                       :policy "doesn't matter for testing"
                       :startdt nil}]
    (with-redefs [proximity-dimming/presence-values (atom {})
                  proximity-dimming/dim-values (atom {})
                  proximity-dimming/send-dimming-command #(do %&)
                  light-utils/get-now #(-> (swap! fake-time inc)
                                           (* 1e6)
                                           proximity-dimming/node-time-to-joda)]

      ;; Simulate starting DD from scratch, which means our
      ;; `dim-values` cache will be empty. Provide Cassandra entries
      ;; for `node-at-minLevel-in-cassandra` , and
      ;; `node-at-maxLevel-in-cassandra`.
      (actlog/log_light_mode (assoc log-light-map
                                    :nodeid [node-at-minLevel-in-cassandra]
                                    :driver minLevel))
      (actlog/log_light_mode (assoc log-light-map
                                    :nodeid [node-at-maxLevel-in-cassandra]
                                    :driver maxLevel))

      ;; Give Cassandra time to write the above entries and be
      ;; available for our PD code.
      (doseq [[nodeid level] {node-at-minLevel-in-cassandra minLevel
                              node-at-maxLevel-in-cassandra maxLevel}]
        (while (not= level
                     (-> nodeid
                         actlog/get_latest_light_mode
                         :driver))
          (Thread/sleep 50)))

      ;; Make sure `lt` is sent appropriately (on change
      ;; only). Perhaps consider writing emulator for this shit? Could
      ;; be in a shared lighting control ns?
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

      ;; Until `run-controller` is evaluated, `dim-values` should
      ;; remain unchanged.
      (is (= {}
             @proximity-dimming/dim-values))

      ;; No prior presence for these nodes. Therefore all four Nodes
      ;; want to be at `minLevel`. However, because
      ;; `node-at-minLevel-in-cassandra` is in Cassandra, it should
      ;; backfill its entry in `dim-values` and not be sent in the
      ;; LFS.
      (is (= [[{:level minLevel
                :nodeid #{node-at-minLevel-not-in-cassandra
                          node-at-maxLevel-not-in-cassandra
                          node-at-maxLevel-in-cassandra}
                :pri 4
                :timeout 0
                :type "LightingForceState"}]
              nil]
             (-> pdprofileid
                 proximity-dimming/get-pdprofile
                 proximity-dimming/run-controller
                 (update 0 vec)
                 (update-in [0 0 :nodeid] set))))

      ;; The nodes that lack an entry in Cassandra have optimistic
      ;; entries in `dim-values`, while the nodes outside Cassandra
      ;; remain untouched, as an `lt` sample is expected to update
      ;; `node-at-maxLevel-in-cassandra`.
      (is (= {node-at-minLevel-not-in-cassandra {:value minLevel
                                                 :time (-> (* 1000000 @fake-time))}
              node-at-maxLevel-not-in-cassandra {:value minLevel
                                                 :time (-> (* 1000000 @fake-time))}
              ;; As of 3.0.4 Greedily update the PD cache to reduce Cassandra writes
              node-at-minLevel-in-cassandra {:value minLevel
                                             :time (-> (* 1000000 @fake-time))}
              node-at-maxLevel-in-cassandra {:value minLevel
                                             :time (-> (* 1000000 @fake-time))}}
             @proximity-dimming/dim-values))

      ;; Manufacture `lt` sensor sample responses.
      (let [t (swap! fake-time inc)]
        (doseq [node [node-at-maxLevel-in-cassandra
                      node-at-maxLevel-not-in-cassandra]]
          (proximity-dimming/handle-lt-value {:nodeid node
                                              :time (-> t
                                                        (* 1000000))
                                              :value minLevel})))

      ;; Confirm state is present in `dim-values`.
      (is (= {node-at-minLevel-not-in-cassandra {:value minLevel
                                                 :time (-> (* 1000000 (dec @fake-time)))}
              node-at-maxLevel-not-in-cassandra {:value minLevel
                                                 :time (-> (* 1000000 @fake-time))}
              ;; As of 3.0.4 Greedily update the PD cache to reduce Cassandra writes
              node-at-minLevel-in-cassandra {:value minLevel
                                             :time (-> (* 1000000 (dec @fake-time)))}
              node-at-maxLevel-in-cassandra {:value minLevel
                                             :time (-> (* 1000000 @fake-time))}}
             @proximity-dimming/dim-values))

      ;; Confirm re-running profile sends no LFS.
      (is (= [nil
              nil]
             (-> pdprofileid
                 proximity-dimming/get-pdprofile
                 proximity-dimming/run-controller))))))

(deftest respect-user-overrides-test
  ;; If a user sends an override, that LFS will be at priority 3,
  ;; higher than the overrides sent by PD, which are priority 4. This
  ;; means PD will be effectively disabled until the user's override
  ;; finally times out.
  (neo4j/executeQuery "MATCH (t:TESTONLY) DETACH DELETE t;")
  (let [orgid (create-test-org)
        siteid (create-test-site orgid)
        [nodeid-overridden
         nodeid-not-overridden] (repeatedly 2 #(create-test-node orgid siteid))
        nodeList [nodeid-overridden
                  nodeid-not-overridden]
        lg (create-test-lighting-group orgid
                                       siteid
                                       {:nodeList nodeList})
        pdprofileid (create-test-pdprofile orgid siteid)
        apply-pdprofile {:type "applyPDtoGroup"
                         :user "uberuser"
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :pdprofileprops {:pdprofileid pdprofileid}
                         :groupprops {:groupids [lg]}}
        {:keys [minLevel
                maxLevel]} default-test-pdprofile

        lfs-level 80

        lighting-force-state {:nodeprops {:type "LightingForceState"
                                          :nodeid [nodeid-overridden]
                                          :level lfs-level
                                          :timeout 0}}


        dev-ctrl-output (atom [])
        dev-ctrl-payload [;; First is LFS for PD to `minLevel`
                          [{"nodeid" (set nodeList)
                            "name" "LightingForceState"
                            "pri" 4
                            "mask" 1
                            "level" minLevel
                            "qualifiers" "undefined"
                            "ftype" "Volatile"}]
                          ;; Second is expected LFS for user override
                          [{"nodeid" [nodeid-overridden]
                            "name" "LightingForceState"
                            "pri" 3
                            "mask" 1
                            "level" lfs-level
                            "qualifiers" "undefined"
                            "ftype" "Volatile"}]
                          ;; Last is LFS for PD to `maxLevel`, but
                          ;; only to `nodeid-not-overridden`
                          [{"nodeid" [nodeid-not-overridden]
                            "name" "LightingForceState"
                            "pri" 4
                            "mask" 1
                            "level" maxLevel
                            "qualifiers" "undefined"
                            "ftype" "Volatile"}]]]

    (with-redefs [proximity-dimming/presence-values (atom {})
                  proximity-dimming/dim-values (atom {})
                  dev-ctrl/sendtodevsvc #(swap! dev-ctrl-output conj %&)]

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

      ;; Run PD and expect `minLevel` sent to both nodes.
      (-> pdprofileid
          proximity-dimming/get-pdprofile
          proximity-dimming/run-controller)
      (is (= (take 1 dev-ctrl-payload)
             (-> dev-ctrl-output
                 (doto (swap! update 0 vec)
                   (swap! update-in [0 0 "nodeid"] set))
                 deref)))

      ;; Send a LFS to just `nodeid-overridden`.
      (dev-ctrl/query-exec-msgpk lighting-force-state)
      (is (= (take 2 dev-ctrl-payload)
             (swap! dev-ctrl-output
                    update
                    1 vec)))

      ;; Report appropriate `lt` for that Node.
      (proximity-dimming/handle-lt-value {:nodeid nodeid-overridden
                                          :time (joda-time-to-node-time (light-utils/get-now))
                                          :value lfs-level})

      ;; When `nodeid-not-overridden` sees presence, it will trigger
      ;; PD, but device control will not send it the LFS, just the
      ;; remaining non-overridden node.
      (proximity-dimming/handle-presence-value {:time 0
                                                :nodeid nodeid-not-overridden
                                                :value 1.0})
      (is (= dev-ctrl-payload
             (swap! dev-ctrl-output
                    update
                    2 vec))))))

(defn wave
  [nodeid]
  (proximity-dimming/handle-presence-value {:value 1.0
                                            :nodeid nodeid
                                            :time (joda-time-to-node-time (light-utils/get-now))})
  (proximity-dimming/handle-presence-value {:value 0.0
                                            :nodeid nodeid
                                            :time (inc (joda-time-to-node-time (light-utils/get-now)))}))

(deftest five-node-test
  (let [fake-time (atom -1)
        orgid (create-test-org)
        siteid (create-test-site orgid)
        nodes [{:name "N013305af"
                :latitude "37.381117"
                :longitude "-121.992678"}
               {:name "N013300e3"
                :latitude "37.381112"
                :longitude "-121.992785"}
               {:name "N013300f0"
                :latitude "37.381034"
                :longitude "-121.992804"}
               {:name "N01233c8a"
                :latitude "37.381198"
                :longitude "-121.992554"}
               {:name "N01334660"
                :latitude "37.38104435821549"
                :longitude "-121.99259203349709"}]
        [node-a
         node-b
         node-c
         node-d
         node-e
         :as nodeList] (mapv #(create-test-node orgid
                                                siteid
                                                %)
                             nodes)
        lg (create-test-lighting-group orgid
                                       siteid
                                       {:nodeList nodeList})
        {:keys [minLevel
                maxLevel]
         :as pdprofile} (assoc default-test-pdprofile
                               :radius 16
                               :mode "radius"
                               :detection_duration 300
                               :minLevel 0
                               :maxLevel 100)
        pdprofileid (create-test-pdprofile orgid
                                           siteid
                                           pdprofile)
        apply-pdprofile {:type "applyPDtoGroup"
                         :user "uberuser"
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :pdprofileprops {:pdprofileid pdprofileid}
                         :groupprops {:groupids [lg]}}

        lfs {"name" "LightingForceState"
             "pri" 4
             "mask" 1
             "qualifiers" "undefined"
             "ftype" "Volatile"}

        dev-ctrl-output (atom [])
        dev-ctrl-expected [;; At time 0, `node-e` will fire PD,
                           ;; sending `node-e` and `node-a` to
                           ;; `maxLevel`, and the rest to `minLevel`.
                           {:time 0
                            :payload [(assoc lfs
                                             "nodeid" #{node-b
                                                        node-c
                                                        node-d}
                                             "level" minLevel)]}
                           {:time 0
                            :payload [(assoc lfs
                                             "nodeid" #{node-e
                                                        node-a}
                                             "level" maxLevel)]}
                           ;; At two minutes in, `node-d` will fire
                           ;; PD, sending `node-d` to `maxLevel`, in
                           ;; addition to `node-a`, `node-e` which are
                           ;; already still there.
                           {:time 120
                            :payload [(assoc lfs
                                             "nodeid" #{node-d}
                                             "level" maxLevel)]}
                           ;; At four and a half minutes, `node-c`
                           ;; will fire PD, sending it to `maxLevel`
                           ;; as well.
                           {:time 260
                            :payload [(assoc lfs
                                             "nodeid" #{node-c
                                                        node-b}
                                             "level" maxLevel)]}
                           ;; At five minutes and 20 seconds,
                           ;; `node-e`'s PD expires.
                           {:time 320
                            :payload [(assoc lfs
                                             "nodeid" #{node-e}
                                             "level" minLevel)]}
                           ;; At seven minutes, 20 seconds, `node-d`'s
                           ;; PD expires.
                           {:time (+ (* 7
                                        60)
                                     20)
                            :payload [(assoc lfs
                                             "nodeid" #{node-d}
                                             "level" minLevel)]}
                           ;; And finally, at nine minutes, 20
                           ;; seconds, `node-c`'s PD expires.
                           {:time (+ (* 9
                                        60)
                                     20)
                            :payload [(assoc lfs
                                             "nodeid" #{node-c
                                                        node-b
                                                        node-a}
                                             "level" minLevel)]}]]

    (is (= (-> {:success true
                :result (assoc pdprofile
                               :pdprofileid pdprofileid
                               :sites []
                               :groups [{:groupid lg
                                         :name "Test Lighting Group"}]
                               :nodes (mapv (fn [{:keys [name]}
                                                 nodeid]
                                              {:name name
                                               :nodeid nodeid})
                                            nodes
                                            nodeList))}
               vectors-to-sets)
           (-> apply-pdprofile
               run
               vectors-to-sets)))

    (with-redefs [proximity-dimming/dim-values (atom {})
                  light-utils/get-now #(-> @fake-time
                                           (* 1e6)
                                           long
                                           proximity-dimming/node-time-to-joda)
                  dev-ctrl/sendtodevsvc (fn [& [{:strs [name
                                                        nodeid
                                                        level]} :as args]]
                                          (when (= "LightingForceState"
                                                   name)
                                            (doseq [nodeid nodeid]
                                              (proximity-dimming/handle-lt-value {:value level
                                                                                  :time (inc (joda-time-to-node-time (light-utils/get-now)))
                                                                                  :nodeid nodeid})))
                                          (swap! dev-ctrl-output conj
                                                 {:time @fake-time
                                                  :payload (update-in (vec args)
                                                                      [0 "nodeid"] set)}))]
      (while (< (swap! fake-time inc)
                601)
        (let [t @fake-time
              node-time (joda-time-to-node-time (light-utils/get-now))]
          (cond
            (= 0 t) (wave node-e)
            (= 120 t) (wave node-d)
            (= 260 t) (wave node-c)
            (zero? (mod (+ 10
                           t)
                        30)) (do
                                 (spy :debug t)
                                 (-> pdprofileid
                                     proximity-dimming/get-pdprofile
                                     proximity-dimming/run-controller
                                     (->> (spy :debug))))))))

    (is (= dev-ctrl-expected
           @dev-ctrl-output))))

(deftest reset-after-endtime
  (with-redefs [proximity-dimming/active-pdprofiles (atom #{})
                proximity-dimming/inactive-pdprofiles (atom #{})]
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          pdprofile (assoc default-test-pdprofile
                           :beginTime "2:00:00"
                           :endTime "3:00:00")
          pdprofileid (create-test-pdprofile orgid
                                             siteid
                                             pdprofile)
          nodeid (create-test-node orgid siteid)
          groupid (create-test-lighting-group orgid
                                              siteid
                                              {:nodeList [nodeid]})]
      (is (= {:success true
              :result (assoc pdprofile
                             :pdprofileid pdprofileid
                             :sites []
                             :groups [{:groupid groupid
                                       :name "Test Lighting Group"}]
                             :nodes [{:nodeid nodeid}])}
             (run {:type "applyPDtoGroup"
                   :user *user*
                   :orgprops {:orgid orgid}
                   :siteprops {:siteid siteid}
                   :pdprofileprops {:pdprofileid pdprofileid}
                   :groupprops {:groupids [groupid]}})))

      ;; Show PDProfile is active.
      (with-redefs [light-utils/get-now (constantly (light-utils/local-time->UTC "2:30:00"
                                                                                 default-test-site))]
        (is (= true
               (-> pdprofileid
                   proximity-dimming/get-pdprofile
                   light-utils/in-schedule)))

        (proximity-dimming/schedule-loop)
        (is (= #{pdprofileid} @proximity-dimming/active-pdprofiles))
        (is (= #{} @proximity-dimming/inactive-pdprofiles))

        (let [dl proximity-dimming/do-lighting
              log (atom [])]
          (with-redefs [proximity-dimming/do-lighting #(do (swap! log conj %&)
                                                           (apply dl %&))]
            (wave nodeid))
          (is (= 1 (count @log)))))

      ;; Now, show the PDProfile is inactive
      (with-redefs [light-utils/get-now (constantly (light-utils/local-time->UTC "3:30:00"
                                                                                 default-test-site))]
        (is (= false
               (-> pdprofileid
                   proximity-dimming/get-pdprofile
                   light-utils/in-schedule)))

        (let [rl light-utils/reset-lights
              log (atom [])]
          (with-redefs [light-utils/reset-lights #(do (swap! log conj %&)
                                                      (apply rl %&))]
            (proximity-dimming/schedule-loop))

          ;; `reset-lights` takes a list (1) as arguments, which we're
          ;; `apply`-ing (2), and appending to our audit log `log` (3).
          ;; Hence, `nodeid` is in three layers of lists.
          (is (= [[[nodeid]]] @log)))

        (is (= #{} @proximity-dimming/active-pdprofiles))
        (is (= #{pdprofileid} @proximity-dimming/inactive-pdprofiles))

        (let [log (atom [])]
          (with-redefs [proximity-dimming/do-lighting #(swap! log conj %&)]
            (wave nodeid))
          (is (= 0 (count @log))))))))

(deftest pdprofile-invalidation
  (with-redefs [light-utils/resolve-symbolic-time :profile]
    (let [orgid (create-test-org)
          siteid (create-test-site orgid)
          pdprofileid (create-test-pdprofile orgid
                                             siteid)
          nodeid (create-test-node orgid siteid)
          lg (create-test-lighting-group orgid
                                         siteid
                                         {:nodeList []})
          get-group (run {:type "getGroup"
                          :user *user*
                          :orgprops {:orgid orgid}
                          :siteprops {:siteid siteid}
                          :groupprops {:groupid lg}})
          cached-pdprofile (assoc default-test-pdprofile
                                  :pdprofileid pdprofileid
                                  :sites []
                                  :groups []
                                  :nodes []
                                  :neighbor-lookup {})]
      (is (= {:success true
              :pdprofile (dissoc cached-pdprofile
                                 :neighbor-lookup)}
             (run {:type "getPDProfile"
                   :user *user*
                   :orgprops {:orgid orgid}
                   :siteprops {:siteid siteid}
                   :pdprofileprops {:pdprofileid pdprofileid}})))
      (is (= cached-pdprofile
             (proximity-dimming/get-pdprofile pdprofileid)))
      (testing "Confirm adding Nodes to a PDProfile"
        (testing "via adding nodes to an existing LG using `updateGroup`"
          (is (= cached-pdprofile
                 (proximity-dimming/get-pdprofile pdprofileid)))
          ;; First, assign the LG the PDProfile
          (is (= (update get-group
                         :group
                         merge
                         {:pdprofiles [{:pdprofileid pdprofileid}]})
                 (run {:type "updateGroup"
                       :user *user*
                       :orgprops {:orgid orgid}
                       :siteprops {:siteid siteid}
                       :groupprops (-> get-group
                                       :group
                                       (merge {:pdprofiles [{:pdprofileid pdprofileid}]}))})))
          (is (= cached-pdprofile
                 (proximity-dimming/get-pdprofile pdprofileid)))
          ;; Second, assign the Node to the Group
          (is (= (update get-group
                         :group
                         merge
                         {:pdprofiles [{:pdprofileid pdprofileid}]
                          :nodeList [nodeid]
                          :nodes [{:model "unode-v4", :nodeid nodeid}]})
                 (run {:type "updateGroup"
                       :user *user*
                       :orgprops {:orgid orgid}
                       :siteprops {:siteid siteid}
                       :groupprops (-> get-group
                                       :group
                                       (merge {:pdprofiles [{:pdprofileid pdprofileid}]
                                               :nodeList [nodeid]}))})))
          (is (= (merge cached-pdprofile
                        {:groups [{:groupid lg
                                   :name "Test Lighting Group"}]
                         :nodes [{:nodeid nodeid}]
                         :neighbor-lookup {nodeid #{}}})
                 (proximity-dimming/get-pdprofile pdprofileid))))
        (testing "via moving Node from an existing LG, using `updateGroup`"
          (let [other-lg (create-test-lighting-group orgid siteid)
                other-pdprofileid (create-test-pdprofile orgid siteid)]
            (is (= {:success true
                    :result (assoc default-test-pdprofile
                                   :pdprofileid other-pdprofileid
                                   :sites []
                                   :groups [{:groupid other-lg
                                             :name "Test Lighting Group"}]
                                   :nodes [])}
                   (run {:type "applyPDtoGroup"
                         :user *user*
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :groupprops {:groupids other-lg}
                         :pdprofileprops {:pdprofileid other-pdprofileid}
                         :nodes [{:model "unode-v4", :nodeid nodeid}]})))
            (let [invalidated (atom [])]
              (with-redefs [proximity-dimming/invalidate-pdprofile-cache-for-id #(swap! invalidated conj %)]
                (is (= (update get-group
                               :group
                               merge
                               {:groupid other-lg
                                :pdprofiles [{:pdprofileid other-pdprofileid}]
                                :nodeList [nodeid]
                                :nodes [{:model "unode-v4", :nodeid nodeid}]})
                       (run {:type "updateGroup"
                             :user *user*
                             :orgprops {:orgid orgid}
                             :siteprops {:siteid siteid}
                             :groupprops (-> get-group
                                             :group
                                             (merge {:groupid other-lg
                                                     :pdprofiles [{:pdprofileid other-pdprofileid}]
                                                     :nodeList [nodeid]}))})))
                (is (= [other-pdprofileid
                        pdprofileid] @invalidated))))
          ;; Finally, reset
          (testing " -- cleanup"
            (is (= get-group
                   (run {:type "updateGroup"
                         :user *user*
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :groupprops (-> get-group
                                         :group)})))
              (proximity-dimming/invalidate-pdprofile-cache-for-id pdprofileid))
            (testing "via removing a Node from the LG with PD using `updateGroup`"
              (let [invalidated (atom [])]
                (with-redefs [proximity-dimming/invalidate-pdprofile-cache-for-id #(swap! invalidated conj %)]
                  (is (= (update get-group
                                 :group
                                 merge
                                 {:groupid other-lg
                                  :pdprofiles [{:pdprofileid other-pdprofileid}]})
                         (run {:type "updateGroup"
                               :user *user*
                               :orgprops {:orgid orgid}
                               :siteprops {:siteid siteid}
                               :groupprops (-> get-group
                                               :group
                                               (merge {:groupid other-lg
                                                       :pdprofiles [{:pdprofileid other-pdprofileid}]}))}))))
                (is (= [other-pdprofileid]
                       @invalidated))))))
        (testing "via adding nodes to an existing LG using `addNodeToGroup`"
          (is (= cached-pdprofile
                 (proximity-dimming/get-pdprofile pdprofileid)))
          ;; First, assign the LG the PDProfile
          (is (= (update get-group
                         :group
                         merge
                         {:pdprofiles [{:pdprofileid pdprofileid}]})
                 (run {:type "updateGroup"
                       :user *user*
                       :orgprops {:orgid orgid}
                       :siteprops {:siteid siteid}
                       :groupprops (-> get-group
                                       :group
                                       (merge {:pdprofiles [{:pdprofileid pdprofileid}]}))})))
          (is (= cached-pdprofile
                 (proximity-dimming/get-pdprofile pdprofileid)))
          (is (= {:success true
                  :result {:groupid lg
                           :nodeids [nodeid]
                           :siteid siteid}}
                 (run {:type "addNodeToGroup"
                       :user *user*
                       :orgprops {:orgid orgid}
                       :siteprops {:siteid siteid}
                       :groupprops {:groupid lg}
                       :nodeprops {:nodeid nodeid}})))
          (is (= (merge cached-pdprofile
                        {:groups [{:groupid lg
                                   :name "Test Lighting Group"}]
                         :nodes [{:nodeid nodeid}]
                         :neighbor-lookup {nodeid #{}}})
                 (proximity-dimming/get-pdprofile pdprofileid))))
        (testing "via adding nodes, already with an LG, to another LG using `addNodeToGroup"
          (let [another-lg (create-test-lighting-group orgid siteid)
                another-pdprofileid (create-test-pdprofile orgid siteid)]
            (is (= {:success true
                    :result (assoc default-test-pdprofile
                                   :pdprofileid another-pdprofileid
                                   :sites []
                                   :groups [{:groupid another-lg
                                             :name "Test Lighting Group"}]
                                   :nodes [])}
                   (run {:type "applyPDtoGroup"
                         :user *user*
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :groupprops {:groupids another-lg}
                         :pdprofileprops {:pdprofileid another-pdprofileid}})))
            (let [invalidated (atom [])]
              (with-redefs [proximity-dimming/invalidate-pdprofile-cache-for-id #(swap! invalidated conj %)]
                (is (= {:success true
                        :result {:groupid another-lg
                                 :nodeids [nodeid]
                                 :siteid siteid}}
                       (run {:type "addNodeToGroup"
                             :user *user*
                             :orgprops {:orgid orgid}
                             :siteprops {:siteid siteid}
                             :groupprops {:groupid another-lg}
                             :nodeprops {:nodeid nodeid}}))))
              (is (= [pdprofileid
                      another-pdprofileid] @invalidated)))
            (testing "and removing Nodes via `removeNodeFromGroup"
              (let [invalidated (atom #{})]
                (with-redefs [proximity-dimming/invalidate-pdprofile-cache-for-id #(swap! invalidated conj %)]
                  (debug "MARK")
                  (is (= {:success true
                          :result {:groupid another-lg
                                   :nodeid nodeid
                                   :siteid siteid}}
                         (run {:type "removeNodeFromGroup"
                               :user *user*
                               :orgprops {:orgid orgid}
                               :siteprops {:siteid siteid}
                               :groupprops {:groupid another-lg}
                               :nodeprops {:nodeid nodeid}}))))
                (is (= #{another-pdprofileid} @invalidated)))))
          (testing "-- cleanup"
            (is (= get-group
                   (run {:type "updateGroup"
                         :user *user*
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :groupprops (-> get-group
                                         :group)})))
            (proximity-dimming/invalidate-pdprofile-cache-for-id pdprofileid)))
        (testing "via adding a PDProfile to an LG with `updateGroup`"
          (is (= cached-pdprofile
                 (proximity-dimming/get-pdprofile pdprofileid)))
          ;; First, assign the node to the LG
          (is (= (update get-group
                         :group
                         merge
                         {:nodeList [nodeid]
                          :nodes [{:model "unode-v4", :nodeid nodeid}]})
                 (run {:type "updateGroup"
                       :user *user*
                       :orgprops {:orgid orgid}
                       :siteprops {:siteid siteid}
                       :groupprops (-> get-group
                                       :group
                                       (merge {:nodeList [nodeid]}))})))
          (is (= cached-pdprofile
                 (proximity-dimming/get-pdprofile pdprofileid)))
          ;; Second, assign the PDProfile to the LG
          (is (= (update get-group
                         :group
                         merge
                         {:pdprofiles [{:pdprofileid pdprofileid}]
                          :nodeList [nodeid]
                          :nodes [{:model "unode-v4", :nodeid nodeid}]})
                 (run {:type "updateGroup"
                       :user *user*
                       :orgprops {:orgid orgid}
                       :siteprops {:siteid siteid}
                       :groupprops (-> get-group
                                       :group
                                       (merge {:pdprofiles [{:pdprofileid pdprofileid}]
                                               :nodeList [nodeid]}))})))
          (is (= (merge cached-pdprofile
                        {:groups [{:groupid lg
                                   :name "Test Lighting Group"}]
                         :nodes [{:nodeid nodeid}]
                         :neighbor-lookup {nodeid #{}}})
                 (proximity-dimming/get-pdprofile pdprofileid))))
        (testing "via adding a PDProfile to an LG, with an existing PDProfile, with `updateGroup`"
          (let [another-pdprofileid (create-test-pdprofile orgid siteid)
                invalidated (atom [])]
            (with-redefs [proximity-dimming/invalidate-pdprofile-cache-for-id #(swap! invalidated conj %)]
              (is (= (update get-group
                             :group
                             merge
                             {:pdprofiles [{:pdprofileid another-pdprofileid}]
                              :nodeList [nodeid]
                              :nodes [{:model "unode-v4", :nodeid nodeid}]})
                     (run {:type "updateGroup"
                           :user *user*
                           :orgprops {:orgid orgid}
                           :siteprops {:siteid siteid}
                           :groupprops (-> get-group
                                           :group
                                           (merge {:pdprofiles [{:pdprofileid another-pdprofileid}]
                                                   :nodeList [nodeid]}))}))))
            (is (= [another-pdprofileid
                    pdprofileid]
                   @invalidated))
            (testing "via removing a PDProfile from an LG, with `updateGroup`"
              (let [invalidated (atom [])]
                (with-redefs [proximity-dimming/invalidate-pdprofile-cache-for-id #(swap! invalidated conj %)]
                  (is (= (update get-group
                                 :group
                                 merge
                                 {:nodeList [nodeid]
                                  :nodes [{:model "unode-v4", :nodeid nodeid}]})
                         (run {:type "updateGroup"
                               :user *user*
                               :orgprops {:orgid orgid}
                               :siteprops {:siteid siteid}
                               :groupprops (-> get-group
                                               :group
                                               (merge {:nodeList [nodeid]}))}))))
                (is (= [another-pdprofileid]
                       @invalidated))))
            (proximity-dimming/invalidate-pdprofile-cache-for-id pdprofileid)))
        (testing "via adding a LG to a PDProfile with `applyPDtoGroup`"
          (is (= cached-pdprofile
                 (proximity-dimming/get-pdprofile pdprofileid)))
          ;; First, assign the node to the LG
          (is (= (update get-group
                         :group
                         merge
                         {:nodeList [nodeid]
                          :nodes [{:model "unode-v4", :nodeid nodeid}]})
                 (run {:type "updateGroup"
                       :user *user*
                       :orgprops {:orgid orgid}
                       :siteprops {:siteid siteid}
                       :groupprops (-> get-group
                                       :group
                                       (merge {:nodeList [nodeid]}))})))
          (is (= cached-pdprofile
                 (proximity-dimming/get-pdprofile pdprofileid)))
          ;; Second, assign the PDProfile to the LG
          (is (= {:success true
                  :result (assoc default-test-pdprofile
                                 :pdprofileid pdprofileid
                                 :sites []
                                 :groups [{:groupid lg
                                           :name "Test Lighting Group"}]
                                 :nodes [{:nodeid nodeid}])}
                 (run {:type "applyPDtoGroup"
                       :user *user*
                       :orgprops {:orgid orgid}
                       :siteprops {:siteid siteid}
                       :groupprops {:groupids lg}
                       :pdprofileprops {:pdprofileid pdprofileid}})))
          (is (= (merge cached-pdprofile
                        {:groups [{:groupid lg
                                   :name "Test Lighting Group"}]
                         :nodes [{:nodeid nodeid}]
                         :neighbor-lookup {nodeid #{}}})
                 (proximity-dimming/get-pdprofile pdprofileid))))
        (testing "via adding LGs, with other existing PDProfiles, to another PDProfile"
          (let [another-pdprofileid (create-test-pdprofile orgid siteid)
                other-nodeid (create-test-node orgid siteid)
                other-lg (create-test-lighting-group orgid
                                                     siteid
                                                     {:nodeList [other-nodeid]})
                other-pdprofileid (create-test-pdprofile orgid siteid)
                invalidated (atom [])]
            (is (= {:success true
                    :result (assoc default-test-pdprofile
                                   :pdprofileid other-pdprofileid
                                   :sites []
                                   :groups [{:groupid other-lg
                                             :name "Test Lighting Group"}]
                                   :nodes [{:nodeid other-nodeid}])}
                   (run {:type "applyPDtoGroup"
                         :user *user*
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :groupprops {:groupids other-lg}
                         :pdprofileprops {:pdprofileid other-pdprofileid}})))
            (with-redefs [proximity-dimming/invalidate-pdprofile-cache-for-id #(swap! invalidated conj %)]
              (is (= (vectors-to-sets
                      {:success true
                       :result (assoc default-test-pdprofile
                                      :pdprofileid another-pdprofileid
                                      :sites []
                                      :groups [{:groupid lg
                                                :name "Test Lighting Group"}
                                               {:groupid other-lg
                                                :name "Test Lighting Group"}]
                                      :nodes [{:nodeid nodeid}
                                              {:nodeid other-nodeid}])})
                     (vectors-to-sets
                      (run {:type "applyPDtoGroup"
                            :user *user*
                            :orgprops {:orgid orgid}
                            :siteprops {:siteid siteid}
                            :groupprops {:groupids [lg
                                                    other-lg]}
                            :pdprofileprops {:pdprofileid another-pdprofileid}})))))
            (is (= [another-pdprofileid
                    other-pdprofileid
                    pdprofileid]
                   @invalidated))
            (proximity-dimming/invalidate-pdprofile-cache-for-id pdprofileid)))))
    (testing "CRUD operations on a PDProfile should invalidate the cache"
      (let [orgid (create-test-org)
            siteid (create-test-site orgid)
            pdprofileid (uuid)
            cached-pdprofile (assoc default-test-pdprofile
                                    :pdprofileid pdprofileid
                                    :sites []
                                    :groups []
                                    :nodes []
                                    :neighbor-lookup {})]
        (testing "on creation"
          (swap! proximity-dimming/pdprofile-cache assoc
                 pdprofileid {:profile "Invalid entry"})
          (is (= "Invalid entry"
                 (proximity-dimming/get-pdprofile pdprofileid)))
          (create-test-pdprofile orgid siteid {:pdprofileid pdprofileid})
          (is (= cached-pdprofile
                 (proximity-dimming/get-pdprofile pdprofileid))))
        (testing "on update"
          (is (= cached-pdprofile
                 (proximity-dimming/get-pdprofile pdprofileid)))
          (is (= {:success true
                  :pdprofile (-> cached-pdprofile
                                 (dissoc :neighbor-lookup)
                                 (assoc :radius 7 :mode "radius"))
                  :site {:latitude "37.380996"
                         :longitude "-121.992299"}}
                 (with-redefs [proximity-dimming/run-controller identity]
                   (run {:user *user*
                         :type "updatePDProfile"
                         :orgprops {:orgid orgid}
                         :siteprops {:siteid siteid}
                         :pdprofileprops (assoc default-test-pdprofile
                                                :pdprofileid pdprofileid
                                                :radius 7 :mode "radius")}))))
          (is (= (assoc cached-pdprofile
                        :radius 7 :mode "radius")
                 (proximity-dimming/get-pdprofile pdprofileid))))
        (testing "on deletion"
          (is (= (assoc cached-pdprofile
                        :radius 7 :mode "radius")
                 (proximity-dimming/get-pdprofile pdprofileid)))
          (is (= {:success true}
                 (run {:user *user*
                       :type "deletePDProfile"
                       :orgprops {:orgid orgid}
                       :siteprops {:siteid siteid}
                       :pdprofileprops {:pdprofileid pdprofileid}})))
          (is (= nil
                 (proximity-dimming/get-pdprofile pdprofileid))))))
    (testing "If a Node has a PDProfile, updating the Node should invalidate the Profile cache"
      (let [orgid (create-test-org)
            siteid (create-test-site orgid)
            pdprofileid (create-test-pdprofile orgid
                                               siteid
                                               {:radius 1 :mode "radius"})
            [node-a node-b
             :as nodeList] (repeatedly 2 #(create-test-node orgid siteid))
            lg (create-test-lighting-group orgid
                                           siteid
                                           {:nodeList nodeList})
            cached-pdprofile (-> default-test-pdprofile
                                 (assoc :radius 1
                                        :mode "radius"
                                        :pdprofileid pdprofileid
                                        :sites []
                                        :groups [{:groupid lg
                                                  :name "Test Lighting Group"}]
                                        :nodes [{:nodeid node-a}
                                                {:nodeid node-b}]
                                        :neighbor-lookup {node-a #{node-a node-b}
                                                          node-b #{node-a node-b}})
                                 vectors-to-sets)
            get-node (run {:type "getNode"
                           :user *user*
                           :orgprops {:orgid orgid}
                           :siteprops {:siteid siteid}
                           :nodeprops {:nodeid node-a}})]
        (is (= {:success true
                :result (dissoc cached-pdprofile
                                :neighbor-lookup)}
               (-> {:type "applyPDtoGroup"
                    :user *user*
                    :orgprops {:orgid orgid}
                    :siteprops {:siteid siteid}
                    :groupprops {:groupids lg}
                    :pdprofileprops {:pdprofileid pdprofileid}}
                   run
                   vectors-to-sets)))
        (is (= cached-pdprofile
               (-> pdprofileid
                   proximity-dimming/get-pdprofile
                   vectors-to-sets)))
        (is (= (-> get-node
                   (update :node dissoc :configStatus)
                   (update :node assoc
                           :pdprofileid pdprofileid
                           :longitude "0"))
               (run {:type "updateNode"
                     :user *user*
                     :orgprops {:orgid orgid}
                     :siteprops {:siteid siteid}
                     :nodeprops (-> get-node
                                    :node
                                    (assoc :longitude "0"))})))
        (is (= (assoc cached-pdprofile
                      :neighbor-lookup {node-a #{node-a}
                                        node-b #{node-b}})
               (-> pdprofileid
                   proximity-dimming/get-pdprofile
                   vectors-to-sets)))))))
