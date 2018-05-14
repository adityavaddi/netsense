(ns common.core
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn])
  (:import (java.util.concurrent ExecutionException)
           (clojure.lang PersistentVector Keyword)
           (java.io FileReader File)))

(def rmerge (partial merge-with merge))
(defn get-config
  ([] (get-config "config.edn"))
  ([filename] (merge-with rmerge
    (edn/read-string (slurp (FileReader. (io/file (io/resource filename)))))
    (if (.exists (new File filename)) (edn/read-string (slurp filename)) {}))))

(defn map-ports [& args]
  (doseq [v args] (assert (= PersistentVector (type v))))
  (let [concatted (into [] (apply concat args))
        start (+ 1 (apply max concatted))
        mapped (range start (+ start (count concatted)))]
    (into [] (map vector concatted mapped))))

(defn future-sequence [fseq]
  (let [deref-timeout 250
        futures (atom fseq)
        deref-one #(deref % deref-timeout %)
        tri #(try (%)
             (catch ExecutionException e (.printStackTrace (.getCause (cast Throwable e)))))
        do-derefs (fn [fseq] (doall (map #(tri (partial deref-one %)) fseq)))
        zip (fn [fseq] (map vector (do-derefs fseq) fseq))
        group (fn [zipped] (group-by #(apply identical? %) zipped))
        unzip #(map first %)
        completed (atom [])]
    (future (while (> (count @futures) 0)
      (let [{finished false progressing true} ((comp group zip) @futures)]
        (swap! futures (fn [_] (unzip progressing)))
        (swap! completed (fn [_] (unzip finished))))) @completed)))

(defn- shift-char [c high low] (str (char (+ (int c) (- (int high) (int low))))))
(defn to-lower-case [c] (if (Character/isUpperCase c) (shift-char c \a \A) c))
(defn to-upper-case [c] (if (Character/isLowerCase c) (shift-char c \A \a) c))
(defn transform-first-char [s transform] (format "%s%s" (transform (first s)) (apply str (rest s))))
(defn capitalize-first [s] (transform-first-char s to-upper-case))
(defn lowercase-first [s] (transform-first-char s to-lower-case))

(defn drop-last-char [s] (apply str (drop-last s)))

(defn single-or-nil [col] (if (= 1 (count col)) (first col)))

(defprotocol Countable
  (pluralize [x])
  (singularize [x]))

(extend String
  Countable
  {:pluralize (fn [noun] (format "%s%s" noun (if (.endsWith noun "s") "es" "s")))
   :singularize (fn [noun] (if (and (.endsWith noun "s") (not (.endsWith noun "ss"))) (drop-last-char noun) noun))})

(extend Keyword
  Countable
  {:pluralize #(keyword (pluralize (name %)))
   :singularize #(keyword (singularize (name %)))})
