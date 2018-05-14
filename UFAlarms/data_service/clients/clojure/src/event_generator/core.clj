(ns event-generator.core
  (:require [clojure.core.typed :as t]
            [clojure.math.numeric-tower :as math]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.core.match :refer [match]]
            [zeromq.zmq :as zmq]
            [msgpack.core :as msgpk]
            [clojure.test.check.generators :as gen])
  (:gen-class))

(defn maybe-negate [b v] (if b (- v) v))
(defn gen-gen-float [tup-gen] (gen/fmap (fn [[b v]] (maybe-negate b (rand v))) tup-gen))
(def gen-float (gen-gen-float (gen/tuple gen/boolean gen/s-pos-int)))
(def gen-pos-float (gen-gen-float (gen/tuple (gen/return false) gen/s-pos-int)))
(def gen-int32 (gen/fmap #(rem % Integer/MAX_VALUE) gen/int)) ;; gen/int actually generates a long (64 bits)
(defn between-one-two [f] (+ 1 (- f (math/floor f))))
(def gen-uint32 (gen/fmap (fn [[i f]] (long (* (math/abs i) (between-one-two f)))) (gen/tuple gen-int32 gen-pos-float)))
(def gen-uint64 (gen/fmap (fn [[i f]] (bigint (* (math/abs i) (between-one-two f)))) (gen/tuple gen-uint32 gen-pos-float)))

(def primitive-generators {
  "bool" gen/boolean
  "bytes" gen/bytes
  "float" gen-float
  "int32" gen-int32
  "int64" gen/int
  "string" gen/string
  "uint32" gen-uint32
  "uint64" gen-uint64 })

(defn file-to-json [file] (json/read (new java.io.FileReader file)))
(def schema (file-to-json (io/file (io/resource "messages/schema.json"))))
(def messages (get schema "messages"))
(def enums (get schema "enums"))
(def device-emissions (file-to-json (io/file (io/resource "messages/emissions.json"))))
(defn single [vctr] (assert (= 1 (count vctr))) (first vctr))
(defn get-by [field col nym] (single (filter #(= nym (get % field)) col)))
(def get-by-name (partial get-by "name"))

(def enum-generators
  (let [get-one-of-gen (fn [values] (gen/one-of (map gen/return (map #(get % "name") values))))]
    (apply hash-map (flatten (map (fn [enum] [(get enum "name") (get-one-of-gen (get enum "values"))]) enums)))))

(def type-generators (merge primitive-generators enum-generators))

(declare gen-gen-message)
(def max-repeats 10)
(def gen-gen-field (memoize (fn [fld]
  (let [field (-> fld (dissoc "options") (dissoc "id"))]
    (assert (= (sort ["rule" "type" "name"]) (sort (keys field))))
    (let [[rule typ nym] [(get field "rule") (get field "type") (get field "name")]
          remove-key (fn [msg-gen] (gen/fmap #(single (vals %)) msg-gen))
          type-gen (if (contains? type-generators typ) (get type-generators typ) (remove-key (gen-gen-message typ)))
          rang (match [rule] ["optional"] [0 1] ["required"] [1 1] ["repeated"] [0 max-repeats])]
      [nym (apply (partial gen/vector type-gen) rang)])))))

(def gen-gen-message (memoize (fn [msg-name]
  (println "Generating message generator for: " msg-name)
  (let [get-message (partial get-by-name messages)
        msg (get-message msg-name)
        fields (get msg "fields")
        field-gens (map gen-gen-field fields)
        vals-mapper (fn [field values] (match [(count values)]
          [0] {}
          [1] {field (single values)}
          :else {field values}))
        fld-json-gens (map (fn [[field val-gen]] (gen/fmap (partial vals-mapper field) val-gen)) field-gens)]
    (gen/fmap (fn [t] {msg-name (into {} t)}) (apply gen/tuple fld-json-gens))))))

(def message-generators (map gen-gen-message device-emissions))
(def message-generator (gen/one-of message-generators))

(defn -main [& args]
  (assert (= clojure.lang.PersistentVector (type device-emissions)))
  (assert (= clojure.lang.PersistentVector (type messages)))
  (let [context (zmq/zcontext)]
    (with-open [requester (doto (zmq/socket context :req) (zmq/connect "tcp://127.0.0.1:5559"))]
      (let [num-reqs 1500
            sample-seq (gen/sample-seq message-generator num-reqs)]
      (dotimes [i num-reqs]
        (zmq/send-str requester (json/write-str {
          :message (json/write-str (first (take 1 (gen/sample message-generator))))
          :timestamp (System/currentTimeMillis)
          :level "INFO"}))
        (let [res (zmq/receive-str requester)]
          (printf (format "Reply #%s: '%s'\n" i res))))))))
