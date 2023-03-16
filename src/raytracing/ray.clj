(ns raytracing.ray
  (:require [raytracing.base :refer :all]
            [raytracing.texture :refer :all])
  (:gen-class))

(defrecord Ray [orig dir time_])

(defn ray
  ([orig dir] (->Ray orig dir 0.0))
  ([orig dir time_] (->Ray orig dir time_)))



(defn get-ray [{:keys [u v horizontal vertical
                       lower-left-corner
                       origin lens-radius time0 time1]}
               ^double s ^double t]
  (let [[x y z] (v* (random-in-unit-disk) lens-radius)
        offset (v+ (v* u x) (v* v y))]
    (ray (v+ origin offset)
         (v- (v+ lower-left-corner
                 (v* horizontal s)
                 (v* vertical t))
             origin
             offset)
         (random-double time0 time1))))
