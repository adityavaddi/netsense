(ns ^{:doc "Proximity Dimming Application."
      :author "Chiradip Mandal"}
    utils.async.async_chans (:gen-class)
    (:require [appl.proximity-dimming :as dimming]
              [appl.daylight_harvest :as dh]
              [appl.et-daylight-harvesting :as etdh]
              [metrics
               [counters :as counters]
               [histograms :as histograms]
               [meters :as meters]
               [timers :as timers]]
              [utils.config :as config]
              [clojure.core.async :as async]
              [clojure.tools.logging :refer :all])
    (:import java.util.concurrent.Executors))

(defonce metric-factory
  (dealer.metrics/metric-factory-stub "utils.async.async_chans"))

;; Some samples
; "jt"  Jolt Threshold
; "mt"
; "l"   Ambient Light
; "lIR" Infrared Sensor
; "t"   Internal Temperature
; "p"   Presence Sensor
; "pc"  Presence Counter
; "T"   Ambient Temperature (mC)
; "v"   Voltage (mV)
; "vp"
; "mi"  Main Current (mA)
; "mip" Main Peak Current
; "ai"  Auxiliary Current (mA)
; "aip" Auxiliary Peak Current
; "mw"  Main Power (mW)
; "aw"  Auxiliary Power (mW)
; "mP"
; "aP"
; "mPF" Main Power Factor
; "aPF" Auxiliary Power Factor
; "lt"  Light Driver Level
; "rf"  Received Signal Strength
; "bc"  Boot Count
; "WDT" Watchdog Timer
; "bR"

(defonce buffer-size (get (config/ddconfig) :async-buffer-size 1000))

(def ^:private presence-detect-chan (async/chan (async/sliding-buffer buffer-size)))

(def ^:private trigger-lux-chan (async/chan (async/sliding-buffer buffer-size)))

(def ^:private ambient-light-chan (async/chan (async/sliding-buffer buffer-size)))

(def ^:private light-driver-level-chan (async/chan (async/sliding-buffer buffer-size)))

(declare process-presence)
(declare process-trigger-lux)
(declare process-ambient-light)
(declare process-light-driver-level)

(defn put-presence-detect [presence-info]
  (async/put! presence-detect-chan presence-info)
  @process-presence)

(defn put-trigger-lux [trigger-lux-info]
  (async/put! trigger-lux-chan trigger-lux-info)
  @process-trigger-lux)

(defn put-ambient-light [ambient-light-info]
  (async/put! ambient-light-chan ambient-light-info)
  @process-ambient-light)

(defn put-light-driver-level [sensor-sample]
  (async/put! light-driver-level-chan sensor-sample)
  @process-light-driver-level)

;; reference
;; sampledata {:nodeid nodeid :sensor sensor :value value :time time}
(defn put-sensor-sample [{:keys [sensor]
                          :as sample}]
  (case sensor
    ;"trgl" (put-trigger-lux sample)
    "p" (do
          (meters/mark! (metric-factory :meter
                                        ["pd-enqueue"]))
          (when (.full? (.buf presence-detect-chan))
            (meters/mark! (metric-factory :meter ["pd-dropped"])))
          (put-presence-detect sample))
    ;"l" (put-ambient-light sample)
    ;"l-i" (put-ambient-light sample) ; Carrier Pigeon
    "lt" (put-light-driver-level sample) ; Carrier Pigeon
    "default"))

(defonce presence-thread-pool-size (get (config/ddconfig) :presence-thread-pool-size 40))

(def process-presence
  (delay
   (async/pipeline-blocking presence-thread-pool-size
                            (doto (async/chan)
                              async/close!)
                            (map (comp (constantly true)
                                       (fn [{:keys [nodeid]
                                             :as sample}]
                                         (meters/mark! (metric-factory :meter
                                                                       ["pd-dequeue"]))
                                         (dimming/handle-presence-value sample))))
                            presence-detect-chan
                            false
                            #(error %))))

(def process-trigger-lux
  (delay
   (async/pipeline 1
                   (doto (async/chan)
                     async/close!)
                   (map (comp (constantly true)
                              (fn [{:keys [nodeid value]
                                    :as sample}]
                                (etdh/handle-trgl-value! sample))))
                   trigger-lux-chan
                   false
                   #(error %))))

(def process-ambient-light
  (delay
   (async/pipeline 1
                   (doto (async/chan)
                     async/close!)
                   (map (comp (constantly true)
                              (fn [{:keys [nodeid value]
                                    :as sample}]
                                (etdh/handle-l-value! sample)
                                ;(dh/record-ambient-light nodeid value)
                                )))
                   ambient-light-chan
                   false
                   #(error %))))

(def process-light-driver-level
  (delay
   (async/pipeline 1
                   (doto (async/chan)
                     async/close!)
                   (map (comp (constantly true)
                              (fn [sample]
                                (dimming/handle-lt-value sample))))
                   light-driver-level-chan
                   false
                   #(error %))))
