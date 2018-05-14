(ns ^{:doc "The Main Dealer."}
    dealer.dealer
  (:gen-class)
  (:require [clojure.tools.logging :refer :all]
            [com.stuartsierra.component :as component]
            [metrics.counters :refer [counter inc! dec!]]
            [msgpack.core :as msgpk]
            [utils.config :as conf]
            [zeromq
             [device :as dev]
             [zmq :as zmq]])
  (:use clojure.tools.logging)
  (:require [utils.config :as conf]
            [msgpack.core :as msgpk]
            [clojure.core.match :refer [match]])
  (:import (org.zeromq ZMQException ZMQ ZMQ$Error ZMsg ZFrame)
           (java.util ArrayDeque)))

(def receiving-zmq-name "dealer-blocking-receive-zmq-sockets")
(defonce receiving-zmq
  (counter receiving-zmq-name))

(def zmqconf (conf/zmqservice))

(def HEARTBEAT "\002")

(defn startproxy
  "Takes a pair of ZeroMQ frontend (router) port and backend (dealer) port and connects them"
  [feport beport]
  (let [context (zmq/zcontext)]
    (.start
     (Thread.
      (fn []
        (try
          (with-open
            [frontend (doto (zmq/socket context :router)
                        (zmq/bind (str "tcp://*:" feport)))
             backend (doto (zmq/socket context :dealer)
                       (zmq/bind (str "tcp://*:" beport)))]
            (dev/proxy context frontend backend))
          (catch Exception e
            (errorf "ZMQ startproxy Exception %s" e)
            (throw e))))))))

(defn startactlogproxy
  "Takes all, json or msgpk as the argument - based on that it selects the proxy(s) to start"
  []
  (let [actlogfeportjs (:fe (:js (:actlog zmqconf)))
        actlogbeportjs (:be (:js (:actlog zmqconf)))]
    (try
      (startproxy actlogfeportjs actlogbeportjs)
      (catch Exception e (throw (Exception. (str (.getMessage e) " : ENC has to be all, json or msgpk")))))))

(defn startdevsvcproxy
  "Takes all, json or msgpk as the argument - based on that it selects the proxy(s) to start"
  []
  (let [devsvcfeportmp (:fe (:mp (:devsvc zmqconf)))
        devsvcbeportmp (:be (:mp (:devsvc zmqconf)))]
    (try
      (startproxy devsvcfeportmp devsvcbeportmp)
      (catch Exception e (throw (Exception. (str (.getMessage e) " : ENC has to be all, json or msgpk")))))))

(defn startdevsvcctrlproxy
  "Takes all, json or msgpk as the argument - based on that it selects the proxy(s) to start"
  []
  (let [feportmp (:fe (:mp (:devsvcctrl zmqconf)))
        beportmp (:be (:mp (:devsvcctrl zmqconf)))]
    (try
      (startproxy feportmp beportmp)
      (catch Exception e (throw (Exception. (str (.getMessage e) " : ENC has to be all, json or msgpk")))))))

(defn startallproxies
  "Start all the proxies with all the encoding JSON and MSGPK"
  []
  (info "Starting all the proxies")
  (startactlogproxy)
  ;(startdevsvcproxy)
  ;(startdevsvcctrlproxy)
  )

(defrecord Dealer [metrics-registry]
  component/Lifecycle
  (start [component]
    (let [{:keys [registry
                  prefix]} metrics-registry]
      (alter-var-root #'receiving-zmq
                      (fn [& _]
                        (counter registry
                                 (conj prefix receiving-zmq-name)))))
    (startallproxies)
    component)
  (stop [component]
    (info "Stopping Dealer.")))

(defn new-dealer
  []
  (map->Dealer {}))
