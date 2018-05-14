(ns mqtt.fixture
  (:require [clojure.tools.logging :refer :all]
            [mqtt
             [core :as mqtt]
             [mock :as mock]]))

(defn mqtt-fixture
  [f]
  (info "Applying mock.")
  (with-redefs [mqtt/mqtt-publish mock/mqtt-publish mqtt/mqtt-subscribe mock/mqtt-subscribe]
    (f)
    (info "Removing mock.")))
