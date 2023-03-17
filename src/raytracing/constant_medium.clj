(ns raytracing.constant-medium
  (:require [raytracing.base :refer :all]
            [raytracing.materials :refer :all]
            [clojure.math :as m])
  (:gen-class))

(set! *unchecked-math* :warn-on-boxed)

(defrecord ConstantMedium [boundary phase-function neg-inv-density]
  Hittable
  (hit [this {:keys [orig dir] :as r} t-min t-max rec]
    (when-let [rec1 (hit boundary r ##-Inf ##Inf nil)]
      (when-let [rec2 (hit boundary r (+ ^double (:t rec1) 0.0001) ##Inf nil)]
        ;(prn "hit skip")
        (let [new-rec1 (cond-> rec1
                               (< ^double (:t rec1)
                                  ^double t-min)
                               (assoc :t t-min))
              new-rec2 (cond-> rec2
                               (> ^double (:t rec2)
                                  ^double t-max)
                               (assoc :t t-max))]
          (when (< ^double (:t new-rec1)
                   ^double (:t new-rec2))
            (let [new-rec1 (if (neg? ^double (:t new-rec1))
                             (assoc new-rec1 :t 0)
                             new-rec1)
                  ray-length ^double (length dir)
                  distance-inside-boundary ^double (* ^double (- ^double (:t new-rec2)
                                                                 ^double (:t new-rec1))
                                                      ^double ray-length)
                  hit-distance ^double (* ^double neg-inv-density
                                          ^double (m/log (random-double)))]
              (when (<= hit-distance
                        distance-inside-boundary)
                (let [t (+ ^double (:t new-rec1)
                           (/ ^double hit-distance
                              ^double ray-length))
                      p (ray-at r t)]
                  (assoc rec
                    :t t
                    :p p
                    :normal [1.0 0.0 0.0]
                    :front-face true
                    :mat-ptr phase-function)))))))))
  (bounding-box [this time0 time1 output-box]
    (bounding-box boundary time0 time1 output-box)))

(defn constant-medium [b
                       ^double d
                       a]
  (->ConstantMedium b
                    (isotropic a)
                    (/ -1 ^double d)))