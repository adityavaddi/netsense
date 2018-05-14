(ns dealer.devsvcctrl-test
  (:require [clojure.test :refer :all]
            [clojure.tools.logging :refer :all]
            [com.stuartsierra.component :as component]
            [dealer.devsvcctrl :as ctrl]
            [mqtt.fixture :as mqtt]
            [utils
             [cassandra_fixture :as cass]
             [neo4j-fixture :as neo4j]]))

(defn mock-send
  [message]
  (debugf "Sending message to device control: %s"
          message)
  message)

(defrecord MockDeviceServiceControl [original-implementation]
  component/Lifecycle
  (start [component]
    (if original-implementation
      (do
        (warn "Not mocking already mocked Device Service Control")
        component)
      (let [original (promise)]
        (debug "Replacing `sendtodevsvc` with a mock.")
        (alter-var-root #'ctrl/sendtodevsvc
                        (fn [real-send]
                          (deliver original real-send)
                          mock-send))
        (assoc component
               :original-implementation @original))))
  (stop [component]
    (if original-implementation
      (do (debug "Reverting mocked `sendtodevsvc` with original implementation.")
          (alter-var-root #'ctrl/sendtodevsvc
                          (fn [my-mock]
                            original-implementation))
          (dissoc component
                  :original-implementation))
      component)))

(defn new-mock-device-service-control
  []
  (map->MockDeviceServiceControl {}))

(defn device-service-control-fixture
  [f]
  (debug "Applying mock.")
  (with-redefs [ctrl/sendtodevsvc mock-send]
    (f)
    (debug "Removing mock.")))

(defn lfs-timeout-fixture
  [f]
  (with-redefs [ctrl/pending-override-resets (atom {})]
    (try
      (f)
      (finally
        (->> ctrl/pending-override-resets
             deref
             vals
             (map future-cancel)
             dorun)))))

(use-fixtures :once (join-fixtures [cass/cassandra-fixture
                                    neo4j/neo4j-fixture
                                    mqtt/mqtt-fixture
                                    device-service-control-fixture
                                    lfs-timeout-fixture]))

(deftest test-lighting-force-state
  (with-redefs [ctrl/active-lfs (atom {})]
    (testing "Lighting Force State"
      (let [lightingForceState {:nodeprops {:type "LightingForceState"
                                            :nodeid ["testnode"]
                                            :level 80
                                            :timeout 0}}
            lfs-expected {"ftype" "Volatile",
                          "level" 80
                          "mask" 1
                          "name" "LightingForceState"
                          "nodeid" ["testnode"]
                          "pri" 3
                          "qualifiers" "undefined"}
            lightingSetAuto {:nodeprops {:type "LightingSetAuto"
                                         :nodeid ["testnode"]}}]
        (testing "should send"
          (is (= lfs-expected
                 (ctrl/query-exec-msgpk lightingForceState)))
          (is (= {"testnode" 3}
                 @ctrl/active-lfs)))
        (testing "should be ignored"
          (is (= nil
                 (ctrl/query-exec-msgpk (update lightingForceState
                                                :nodeprops
                                                assoc :pri 4))))
          (is (= {"testnode" 3}
                 @ctrl/active-lfs)))
        (testing "should send"
          (is (= (assoc lfs-expected
                        "pri" 2)
                 (ctrl/query-exec-msgpk (update lightingForceState
                                                :nodeprops
                                                assoc :pri 2))))
          (is (= {"testnode" 2}
                 @ctrl/active-lfs)))
        (testing "after clear should send"
          (is (= {"name" "LightingSetAuto"
                  "nodeid" ["testnode"]}
                 (ctrl/query-exec-msgpk lightingSetAuto)))
          (is (= {}
                 @ctrl/active-lfs))
          (is (= lfs-expected
                 (ctrl/query-exec-msgpk lightingForceState)))
          (is (= {"testnode" 3}
                 @ctrl/active-lfs)))))))

(deftest test-light-override-lww
  (testing "When `prepare-devsvc-command` is sent a `LightingForceState`"
    (testing "confirm it produces an entry in `pending-override-resets`"
      (let [nodeids ["x"]]
        (with-redefs [ctrl/pending-override-resets (atom {})]
          (ctrl/prepare-devsvc-command {:nodeprops {:type "LightingForceState"
                                                    :nodeid nodeids
                                                    :level 100
                                                    :timeout 100000}})
          (is (= nodeids
                 (keys @ctrl/pending-override-resets)))
          (->> (vals @ctrl/pending-override-resets)
               (map future-cancel)
               dorun))))
    (testing "confirm a following `LightingForceState` overrides the previous entry in `pending-override-resets`"
      (let [nodeid "x"
            nodeids [nodeid]]
        (with-redefs [ctrl/pending-override-resets (atom {})]
          (ctrl/prepare-devsvc-command {:nodeprops {:type "LightingForceState"
                                                    :nodeid nodeids
                                                    :level 100
                                                    :timeout 100000}})
          (is (= nodeids
                 (keys @ctrl/pending-override-resets)))
          (let [f (get @ctrl/pending-override-resets
                       nodeid)]
            (ctrl/prepare-devsvc-command {:nodeprops {:type "LightingForceState"
                                                      :nodeid nodeids
                                                      :level 100
                                                      :timeout 100000}})
            (is (= nodeids
                   (keys @ctrl/pending-override-resets)))
            (is (not= f
                      (get @ctrl/pending-override-resets
                           nodeid))))
          (->> (vals @ctrl/pending-override-resets)
               (map future-cancel)
               dorun))))))
