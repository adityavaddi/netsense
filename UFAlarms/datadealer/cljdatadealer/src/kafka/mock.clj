(ns kafka.mock
  (:require [clojure.tools.logging :refer :all]
            [kafka
             [producer :as kafka-producer]]))

(defn send-to-producer
  "Send a message to Kafka"
  [topic msg]
  (debugf "send-to-producer (%s): %s" topic msg))

(defn kafka-fixture
  [f]
  (info "Applying mock.")
  (with-redefs [kafka-producer/send-to-producer send-to-producer]
    (f)
    (info "Removing mock.")))

