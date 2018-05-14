(ns appl.et-daylight-harvesting.fsm-test
  (:require [appl.et-daylight-harvesting.fsm :as sut]
            [clojure.test :refer :all]
            [reduce-fsm :as fsm]))

;; #6
(deftest fsm-test
  (testing "FSM starts in fast-polling state"
    (is (= :fast-polling
           (-> (sut/etdh-fsm)
               :state))))
  (testing "FSM transitions to"
    (testing "slow-polling state"
      (binding [sut/*fsm-transition* (promise)]
        (is (= :slow-polling
               (-> (sut/etdh-fsm)
                   (fsm/fsm-event :dim-light)
                   :state)))
        (is (= true
               (realized? sut/*fsm-transition*)))
        (is (= :slow-polling
               (and (realized? sut/*fsm-transition*)
                    @sut/*fsm-transition*)))))
    (testing "hour-polling state"
      (binding [sut/*fsm-transition* (promise)]
        (is (= :hour-polling
               (-> (sut/etdh-fsm)
                   (fsm/fsm-event :dim-light-and-late)
                   :state)))
        (is (= true
               (realized? sut/*fsm-transition*)))
        (is (= :hour-polling
               (and (realized? sut/*fsm-transition*)
                    @sut/*fsm-transition*)))))
    (testing "no-polling state"
      (binding [sut/*fsm-transition* (promise)]
        (is (= :no-polling
               (-> (sut/etdh-fsm)
                   (fsm/fsm-event :high-trigger)
                   :state)))
        (is (= true
               (realized? sut/*fsm-transition*)))
        (is (= :no-polling
               (and (realized? sut/*fsm-transition*)
                    @sut/*fsm-transition*)))))))
