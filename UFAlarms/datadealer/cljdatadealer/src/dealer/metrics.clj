(ns dealer.metrics
  (:require [clj-message-digest.core :refer [sha-1-hex]]
            [clj-time.core :as time-core]
            [clojure.tools.logging :refer :all]
            [com.stuartsierra.component :as component]
            [environ.core :as environ]
            [metrics.core :as metrics]
            [metrics.counters :as counters]
            [metrics.histograms :as histograms]
            [metrics.meters :as meters]
            [metrics.timers :as timers]
            [metrics.jvm.core :as jvm]
            [metrics.reporters
             [graphite :as graphite]
             [jmx :as jmx]])
  (:import com.codahale.metrics.MetricFilter
           java.util.concurrent.TimeUnit))

(def metric-map
  {:timer timers/timer
   :counter counters/counter
   :meter meters/meter
   :histogram histograms/histogram})

(defn metric-factory-stub
  [namespace]
  (fn metric-factory
    [metric-type
     names]
    {:pre [(coll? names)]}
    (let [metric (get dealer.metrics/metric-map
                      metric-type)]
      (assert metric)
      (metric (apply conj [namespace]
                     names)))))

(defn metric-factory-factory
  [namespace
   registry
   prefix]
  (fn metric-factory
    [metric-type
     names]
    {:pre [(coll? names)]}
    (let [metric (get dealer.metrics/metric-map
                      metric-type)]
      (assert metric)
      (metric registry
              (apply conj prefix
                     namespace
                     names)))))

(defn rand-name []
  (-> (time-core/now)
      str
      sha-1-hex
      (.substring 1 8)))

(defrecord MetricsRegistry [registry
                            service
                            hostname
                            prefix]
  component/Lifecycle
  (start [component]
    (info "Creating MetricsRegistry.")
    (let [registry (metrics/new-registry)
          hostname (:java-rmi-server-hostname environ/env)
          hostname (str
                     service "-" (rand-name) "-"  ; Need unique for multiple instances.
                     (if (and hostname
                            (not= "" hostname))
                      hostname
                      (:env-suffix environ/env (rand-name))))
          prefix [hostname "datadealer"]]
      (infof "Using hostname: %s" hostname)
      (assoc component
             :registry registry
             :service service
             :hostname hostname
             :prefix prefix)))
  (stop [component]
    (dissoc component
            :registry
            :service
            :hostname
            :prefix)))

(defn new-metrics-registry
  [service]
  (map->MetricsRegistry {:service service}))

(defrecord JMXReporter [reporter
                        metrics-registry]
  component/Lifecycle
  (start [component]
    (info "Starting JMX reporter.")
    (let [{:keys [registry]} metrics-registry
          reporter (jmx/reporter registry {})]
      (jmx/start reporter)
      (assoc component
             :reporter reporter)))
  (stop [component]
    (info "Stopping JMX reporter.")
    (when reporter
      (jmx/stop reporter))
    (dissoc component
            :reporter)))

(defn new-jmx-reporter
  []
  (map->JMXReporter {}))

(defrecord GraphiteReporter [host reporter metrics-registry]
  component/Lifecycle
  (start [component]
    (if host
      (do
        (infof "Starting Graphite reporter on host: %s" host)
        (let [{:keys [registry]} metrics-registry
              reporter (graphite/reporter registry
                                          {:host host
                                           :rate-unit TimeUnit/SECONDS
                                           :duration-unit TimeUnit/MILLISECONDS
                                           :filter MetricFilter/ALL})]
          (graphite/start reporter 5)
          (assoc component
                 :reporter reporter)))
      (do
        (info "No host provided. Not starting Graphite reporter.")
        component)))
  (stop [component]
    (info "Stopping Graphite reporter.")
    (when reporter
      (graphite/stop reporter))
    (dissoc component
            :reporter)))

(defn new-graphite-reporter
  [{:keys [host]
    :as config}]
  (map->GraphiteReporter {:host host}))

(defrecord JVMInstrumenter [metrics-registry]
  component/Lifecycle
  (start [component]
    (info "Instrumenting JVM metrics.")
    (let [{:keys [registry
                  prefix]} metrics-registry]
      (->> {jvm/register-jvm-attribute-gauge-set "attribute"
            jvm/register-memory-usage-gauge-set "memory"
            jvm/register-file-descriptor-ratio-gauge-set "file"
            jvm/register-garbage-collector-metric-set "gc"
            jvm/register-thread-state-gauge-set "thread"}
           (map (fn [[function title]]
                  (function registry
                            (conj prefix "jvm" title))))
           dorun))
    component)
  (stop [component]
    component))

(defn new-jvm-instrumenter
  []
  (map->JVMInstrumenter {}))
