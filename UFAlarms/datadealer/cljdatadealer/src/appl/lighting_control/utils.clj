(ns appl.lighting-control.utils
  (:require [clj-time.core :as clj-time]
            [clojure.tools.logging :refer :all]
            [dealer.devsvcctrl :as dev-ctrl]
            [metrics.timers :as timers]
            [utils.solartime :as sol]))

(defonce metric-factory
  (dealer.metrics/metric-factory-stub "appl.lighting-control.utils"))

(defn get-now
  "A trivial function split out so time can be mocked in the unit
  tests with `with-redefs`."
  []
  (clj-time/now))

(defn local-time->UTC
  ([localtime coordinates]
   (local-time->UTC localtime coordinates (get-now)))
  ([localtime {:keys [latitude longitude]} date]
   ((sol/resolve-expr localtime)
    latitude
    longitude
    (clj-time/to-time-zone date
                           (sol/get-time-zone latitude longitude)))))

(defn resolve-symbolic-time
  [{:keys [site]
    {:keys [scheduled
            beginTime endTime]
     :as profile} :profile}]
  (with-open [_ (timers/start (metric-factory :timer
                                              ["resolve-symbolic-time"]))]
    (cond-> profile
      (and beginTime
           endTime) (assoc
                     :beginTime (local-time->UTC beginTime site)
                     :endTime (local-time->UTC endTime site))
      (seq scheduled) (update
                       :scheduled (fn [scheduled]
                                    (map #(resolve-symbolic-time {:site site
                                                                  :profile %})
                                         scheduled))))))

(defn interval-check
  [begin now end]
  ;; If any timestamp matches any other, `interval-check` returns true.
  (if-not (distinct? begin
                     now
                     end)
    true
    (let [interval (cond
                     (clj-time/before? begin end) (clj-time/interval begin
                                                                     end)
                     (clj-time/after? now begin) (clj-time/interval begin
                                                                    (clj-time/plus end
                                                                                   (clj-time/days 1)))
                     (clj-time/before? now end) (clj-time/interval (clj-time/minus begin
                                                                                   (clj-time/days 1))
                                                                   end)
                     :else (debug (ex-info "Unhandled interval wraparound cornercase"
                                          {:begin begin
                                           :now now
                                           :end end})))]
      (if interval
        (clj-time/within? interval
                          now)
        false))))

(defn in-schedule
  "A `beginTime` and `endTime` is mandatory for both DH and PD. If not
  present, log a `WARN` and don't return true."
  [{:keys [beginTime endTime
           scheduled]
    :as profile}]
  (cond
    (and beginTime
         endTime) (let [now (get-now)]
                    (interval-check beginTime
                                    now
                                    endTime))
    (seq scheduled) (let [now (get-now)]
                      (true? (some (fn [{:keys [beginTime
                                                endTime]}]
                                     (interval-check beginTime
                                                     now
                                                     endTime))
                                   scheduled)))
    :else (warn (ex-info "Lacking either `beginTime`, `endTime`, or both."
                         (if profile
                           profile
                           {})))))

(defn atom?
  [a]
  (instance? clojure.lang.Atom a))

(defn atom-set-move
  "Given a list of items, remove from `from`, and add to `to`."
  [elements atom-set-from atom-set-to]
  {:pre [(coll? elements)
         (atom? atom-set-from)
         (atom? atom-set-to)]}
  (when (seq elements)
    (apply swap! atom-set-from disj elements)
    (apply swap! atom-set-to conj elements)))

(defn reset-lights
  [nodeids]
  {:pre [(coll? nodeids)]}
  (let [nodeids (filter (fn [nodeid]
                          (when-let [priority (get @dev-ctrl/active-lfs nodeid)]
                            (= 4
                               priority)))
                        nodeids)]
    (debugf "Reset nodes back to schedule, removing overrides on nodes: %s"
           (vec nodeids))
    (dev-ctrl/query-exec-msgpk {:nodeprops {:type "LightingSetAuto"
                                            :nodeid nodeids}})))
