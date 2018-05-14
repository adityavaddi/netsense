(ns utils.nodeconfig-test
  (:require [utils.nodeconfig :as sut]
            [clojure.test :refer :all]))

;; Neo4j Nodes accept non-nested JSON maps as a representation of
;; their properties. This means we can't store keys containing `.` or
;; `-`, so we encode them to `_` before storing in Neo4j, and decode
;; when extracting from Neo4j.
(deftest neo4j-encoding-test
  (doseq [[_ config-map] (dissoc sut/sensors
                                 :sensor-name-translator)
          :let [encoded (sut/encode-config config-map)
                decoded (sut/translate-config encoded)]]
    ;; Encoding and then immediately decoding should be the same as
    ;; doing nothing to the `config-map` at all.
    (is (= config-map
           decoded))
    ;; None of the keys from the `encoded` map should match any of the
    ;; keys from the original `config-map`, once entries not
    ;; containing a `.` or `-` are removed.
    (is (= []
           (filter (->> (keys config-map)
                        ;; Only keep keys from `config-map` that have
                        ;; a `.` or `-`.
                        (filter (fn [key]
                                  (some #{\. \-} (name key))))
                        set)
                   (keys encoded))))))

;; Keep the exception list small by testing that there are no
;; unnecessary pairs present.
(deftest no-unnecessary-translations-test
  (is (= {}
         (->> (:sensor-name-translator sut/sensors)
              (filter (fn [[neo4j node]]
                        (= (name neo4j)
                           (-> (name node)
                               (.replace \. \_)))))
              (into {})))))
