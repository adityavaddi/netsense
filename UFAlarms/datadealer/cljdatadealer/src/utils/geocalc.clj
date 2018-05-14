(ns utils.geocalc
  (:gen-class)
  (:require [clojure.tools.logging :refer :all]))

;;; To calculate the distance between two lat/lon pairs, we assume the
;;; earth is not an ellipsoid and calcualte the distance between two
;;; points on a sphere. This is called the haversine equation.
;;;
;;; However, a simpler approximation can be applied when the earth is
;;; flattened onto rectangular coordinates. This is called
;;; equirectangular projection.
;;;
;;; See: http://www.movable-type.co.uk/scripts/latlong.html
;;;      https://en.wikipedia.org/wiki/Equirectangular_projection

(def radius-of-earth
  "In meters."
  (* 6371
     1e3))

(def to-radians
  #(Math/toRadians %))

(defn haversine-approximation
  " var x = (λ2-λ1) * Math.cos((φ1+φ2)/2);
    var y = (φ2-φ1);
    var d = Math.sqrt(x*x + y*y) * R;
  "
  [{lat1 :lat
    lon1 :lon}
   {lat2 :lat
    lon2 :lon}]
  {:pre [lat1 lon1
         lat2 lon2]}
  (let [[lat1 lon1
         lat2 lon2] (map to-radians
                         [lat1 lon1
                          lat2 lon2])
        x (* (- lon2 lon1)
             (Math/cos (/ (+ lat1 lat2)
                          2)))
        y (- lat2 lat1)
        distance (* (Math/sqrt (+ (* x x)
                                  (* y y)))
                    radius-of-earth)]
    distance))

(defn proximity
  [point
   radius
   groupofpoints]
  (filter (fn [candidate-point]
            (< (haversine-approximation point candidate-point)
               radius))
          groupofpoints))
