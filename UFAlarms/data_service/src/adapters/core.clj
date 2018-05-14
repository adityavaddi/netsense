(ns adapters.core
  (:require [clojure.set]))

(comment (typed/ann-record LogEvent
  [ app-start-time :- Integer
    app-name :- String
    host-name :- String
    host-ip :- String
    class-name :- String
    file-name :- String
    level :- String
    line-number :- String
    timestamp :- Integer
    logger-class-name :- String
    message :- String
    method-name :- String
    ndc :- String
    thread-name :- String
    throwable-str-repr :- String ]))

(defrecord LogEvent [app-start-time app-name host-name host-ip class-name file-name level
    line-number timestamp logger-class-name message method-name ndc thread-name throwable-str-repr])

(comment (typed/ann-record UniqueLogEvent
  [ log-event :- LogEvent
    id :- java.util.UUID ]))

(defrecord UniqueLogEvent [log-event id])

(defprotocol EventConsumer
  (consume [this event]))

(defn consume-log-event [consumer log-event]
  (let [unique-log-event (UniqueLogEvent. log-event (java.util.UUID/randomUUID))]
    (consume consumer unique-log-event)))

(defn dash-to-underscore [m]
  (clojure.set/rename-keys m (apply array-map
    (mapcat (fn [key] [key (keyword (clojure.string/replace (name key) #"-" "_"))]) (keys m)))))

(defn flatten-uniqueLogEvent [unique-log-event]
  (dash-to-underscore (into {} [{:key (:id unique-log-event)} (:log-event unique-log-event)])))
