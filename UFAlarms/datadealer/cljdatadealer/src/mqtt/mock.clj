(ns mqtt.mock
  (:gen-class)
  (:require [clojure.tools.logging :refer :all]
            [mqtt.core :as mqtt]
            [com.stuartsierra.component :as component]))

(defn mqtt-publish [topic-name message-string client-id]
  (debugf "Publishing to topic [%s] with message: %s"
          topic-name
          message-string))

(defn mqtt-subscribe [topic-name callback]
  (debugf "Subscribing to topic [%s]"
          topic-name))

(defrecord MockMQTT [original-publish
                     original-subscribe]
  component/Lifecycle
  (start [component]
    (cond
      original-publish (do
                         (warn "Not mocking already mocked MQTT")
                         component)
      original-subscribe (do
                           (warn "Not mocking already mocked MQTT")
                           component)
      :esle (let [publish (promise)
                  subscribe (promise)]
              (info "Replacing `mqtt-publish` with a mock.")
              (alter-var-root #'mqtt/mqtt-publish
                              (fn [real-publish]
                                (deliver publish real-publish)
                                mqtt-publish))
              (alter-var-root #'mqtt/mqtt-subscribe
                              (fn [real-subscribe]
                                (deliver subscribe real-subscribe)
                                mqtt-subscribe))
              (assoc component
                     :original-publish @publish
                     :original-subscribe @subscribe))))
  (stop [component]
    (if original-publish
      (do (info "Reverting mocked `mqtt-publish` with original implementation.")
          (alter-var-root #'mqtt/mqtt-publish
                          (fn [my-mock]
                            original-publish))
          (dissoc component
                  :original-publish))
      component)))

(defn new-mock-mqtt
  []
  (map->MockMQTT {}))
