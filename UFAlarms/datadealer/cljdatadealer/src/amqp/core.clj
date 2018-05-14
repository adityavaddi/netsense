(ns amqp.core
  (:require [clojure.tools.logging :refer :all]
            [com.stuartsierra.component :as component]
            [langohr.core :as rmq]))

(defn close-channel
  [channel]
  (when channel
    (try
      (.close channel)
      (catch com.rabbitmq.client.AlreadyClosedException _
        (warn "Channel was already closed")))))

(defn connect
  [config]
  (loop [delay 250]
    (let [connection-attempt (try
                               (rmq/connect config)
                               (catch java.net.ConnectException e
                                 :connect-exception)
                               (catch java.io.IOException e
                                 :io-exception))]
      (cond
        (instance? com.novemberain.langohr.Connection connection-attempt) (spyf :info
                                                                                "Connected to RabbitMQ: %s"
                                                                                connection-attempt)
        (= connection-attempt
           :connect-exception) (do
                                 (infof "Could not connect. Sleeping %d, then retrying."
                                        delay)
                                 (Thread/sleep delay)
                                 (recur (* 2 delay)))
        (= connection-attempt
           :io-exception) (do
                            (infof "Could not connect. Sleeping %d, then retrying."
                                   delay)
                            (Thread/sleep delay)
                            (recur (* 2 delay)))
        :else (throw (ex-info "Could not connect to RMQ"
                              {:connection connection-attempt}))))))

(defrecord AMQPConnection [config
                           connection]
  component/Lifecycle
  (start [component]
    (infof "Connecting to %s AMQP connection."
           config)
    (let [config (select-keys config
                              [:host
                               :username
                               :password
                               :executors])]
      (-> component
          (assoc :connection
                 (connect config)))))
  (stop [component]
    (info "Terminating AMQP connection.")
    (rmq/close connection)
    (info "Terminated.")))

(defn new-amqp-connection
  [config]
  (map->AMQPConnection {:config config}))
