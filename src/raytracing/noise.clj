(ns raytracing.noise
  (:require [raytracing.base :refer :all]
            [clojure.math :as m])
  (:gen-class))

(defprotocol Noise
  (noise [this point])
  (turb [this p depth]))

(defn trilinear-interp [c ^double u ^double v ^double w]
  (->> (for [^double i [0 1]
             ^double j [0 1]
             ^double k [0 1]]
         (* ^double (+ (* i u) (* (- 1 i) (- 1 u)))
            ^double (+ (* j v) (* (- 1 j) (- 1 v)))
            ^double (+ (* k w) (* (- 1 k) (- 1 w)))
            ^double (get-in c [i j k])))
       (reduce +)))

(defn perlin-interp [c ^double u ^double v ^double w]
  (let [[uu vv ww] (mapv #(* ^double %
                             ^double %
                             ^double (- 3.0 (* 2.0 ^double %))) [u v w])]
    (->> (for [^double i [0 1]
               ^double j [0 1]
               ^double k [0 1]]
           (let [weight-v [(- u i) (- v j) (- w k)]]
             (* ^double (+ ^double (* i ^double uu) ^double (* (- 1 ^double i) ^double (- 1 ^double uu)))
                ^double (+ ^double (* j ^double vv) ^double (* (- 1 ^double j) ^double (- 1 ^double vv)))
                ^double (+ ^double (* k ^double ww) ^double (* (- 1 ^double k) ^double (- 1 ^double ww)))
                ^double (dot (get-in c [i j k]) weight-v))))
         (reduce +))))

(defrecord Perlin [^long point-count
                   ranvec
                   perm-x perm-y perm-z]
  Noise
  (noise [{:keys [ranvec perm-x perm-y perm-z]} point]
    (let [[u v w] (mapv (fn [^double n]
                          (let [value ^double (- n ^double (m/floor n))]
                            (* ^double value
                               ^double value
                               (- 3 ^double (* 2.0 ^double value)))))
                        point)
          [i j k] (mapv #(long (m/floor %)) point)
          c (->> (for [di [0 1]
                       dj [0 1]
                       dk [0 1]]
                   [di dj dk])
                 (reduce (fn [acc [di dj dk]]
                           (assoc-in acc [di dj dk]
                                     (ranvec (bit-or ^long (perm-x (bit-and (+ ^long i ^long di) 255))
                                                     ^long (perm-y (bit-and (+ ^long j ^long dj) 255))
                                                     ^long (perm-z (bit-and (+ ^long k ^long dk) 255))))))
                         [[[0 0] [0 0]]
                          [[0 0] [0 0]]]))]
      (perlin-interp c u v w)))
  (turb [this point depth]
    (loop [i 0
           acc 0.0
           weight 1.0
           temp-p point]
      (if (= i depth)
        (abs acc)
        (recur (inc i)
               (+ acc (* ^double weight
                         ^double (noise this temp-p)))
               (* weight 0.5)
               (v* temp-p 2))))))

(defn perlin-generate-perm []
  (->> (range 256)
       shuffle
       vec))

(defn perlin []
  (let [point-count 256]
    (->Perlin point-count
              (vec (repeatedly point-count #(unit-vector (random-vec3 -1 1))))
              (perlin-generate-perm)
              (perlin-generate-perm)
              (perlin-generate-perm))))