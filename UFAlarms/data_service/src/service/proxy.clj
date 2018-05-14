(ns service.proxy (:gen-class)
  (:require [zeromq [device :as dev] [zmq :as zmq]]
            [common.core]))

(defn start-proxy [upstream-port downstream-port]
  (let [context (zmq/zcontext) poller (zmq/poller context 2)]
    (with-open
      [frontend (doto (zmq/socket context :router)
        (zmq/bind (format "tcp://*:%s" upstream-port)))
      backend (doto (zmq/socket context :dealer)
        (zmq/bind (format "tcp://*:%s" downstream-port)))]
      (dev/proxy context frontend backend))))

(def config (delay (common.core/get-config)))

(defn -main []
  (let [msgpack-ports (get-in @config [:listen-ports :backend :msgpack])
        plaintxt-ports (get-in @config [:listen-ports :backend :plaintxt])
        frontend-ports (get-in @config [:listen-ports :frontend])
        port-pairs (common.core/map-ports msgpack-ports plaintxt-ports frontend-ports)
        futures (map (fn [[upstream downstream]] (future (start-proxy upstream downstream))) port-pairs)]
    (println (format "Service.Proxy started. Forwarding ports: %s"  port-pairs))
    (let [fut (common.core/future-sequence futures)] @fut)))
