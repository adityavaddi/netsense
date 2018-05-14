(ns ^{:doc "Event Triggered Daylight Harvesting Finite State Machine"}
    appl.et-daylight-harvesting.fsm
  (:require [reduce-fsm :as fsm]
            [clojure.tools.logging :refer :all]))

;; See: https://xeranet.atlassian.net/wiki/pages/viewpage.action?pageId=123945511
;;      https://xeranet.atlassian.net/wiki/pages/viewpage.action?pageId=122966762

;; On each transition of the ETDH FSM, deliver on a dynamically bound
;; scoped promise. This allows isolation to the set of transition
;; functions.
(def ^:dynamic *fsm-transition*
  nil)

(defn fsm-transition
  [{:keys [etdhprofileid]
    :as acc} event current-state target-state]
  {:pre [(some? *fsm-transition*)
         (instance? clojure.lang.IPending *fsm-transition*)
         (not (realized? *fsm-transition*))]}
  (debugf "ETDHProfile %s in state %s received %s. Moving to state %s"
         etdhprofileid
         current-state
         event
         target-state)
  (deliver *fsm-transition* target-state)
  acc)

(def etdh-fsm
  (fsm/fsm-inc [[:fast-polling
                 :dim-light          -> {:action fsm-transition} :slow-polling
                 :dim-light-and-late -> {:action fsm-transition} :hour-polling
                 :high-trigger       -> {:action fsm-transition} :no-polling]
                [:slow-polling
                 :low-trigger        -> {:action fsm-transition} :fast-polling
                 :undim-light        -> {:action fsm-transition} :fast-polling
                 :sunrise-90         -> {:action fsm-transition} :fast-polling
                 :sunset-90          -> {:action fsm-transition} :fast-polling

                 :dim-light-and-late -> {:action fsm-transition} :hour-polling
                 :high-trigger       -> {:action fsm-transition} :no-polling]
                [:hour-polling
                 :low-trigger        -> {:action fsm-transition} :fast-polling
                 :undim-light        -> {:action fsm-transition} :fast-polling
                 :sunrise-90         -> {:action fsm-transition} :fast-polling
                 :sunset-90          -> {:action fsm-transition} :fast-polling

                 :high-trigger       -> {:action fsm-transition} :no-polling]
                [:no-polling
                 :low-trigger        -> {:action fsm-transition} :fast-polling
                 :sunrise-90         -> {:action fsm-transition} :fast-polling
                 :sunset-90          -> {:action fsm-transition} :fast-polling]]))

(def fsm-event
  fsm/fsm-event)
