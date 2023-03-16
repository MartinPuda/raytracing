(ns raytracing.rotate
  (:require [raytracing.base :refer :all]
            [clojure.math :as m])
  (:gen-class))

(defrecord RotateY [ptr sin-theta cos-theta hasbox bbox]
  Hittable
  (hit [this {:keys [orig dir time_]} t-min t-max rec]
    (let [[^double ox
           ^double oy
           ^double oz] orig
          [^double dx
           ^double dy
           ^double dz] dir
          new-origin [(- (* cos-theta ox)
                         (* sin-theta oz))
                      oy
                      (+ (* sin-theta ox)
                         (* cos-theta oz))]
          new-direction [(- (* cos-theta dx)
                            (* sin-theta dz))
                         dy
                         (+ (* sin-theta dx)
                            (* cos-theta dz))]
          rotated-r (ray new-origin new-direction time_)]
      (when-let [{:keys [p normal] :as hit-rec}
                 (hit ptr rotated-r t-min t-max rec)]
        (let [[px py pz] p
              new-p [(+ (* cos-theta px)
                        (* sin-theta pz))
                     py
                     (+ (* sin-theta px)
                        (* cos-theta pz))]
              [nx ny nz] normal
              new-normal [(+ (* cos-theta nx)
                             (* sin-theta nz))
                          ny
                          (+ (* sin-theta nx)
                             (* cos-theta nz))]
              {:keys [front-face normal]} (set-face-normal rotated-r new-normal)]
          (-> hit-rec
              (assoc :p new-p
                     :front-face front-face
                     :normal normal))))))
  (bounding-box [this time0 time1 output-box]
    bbox))

(defn rotate-y [p angle]
  (let [radians (m/to-radians angle)
        sin-theta (m/sin radians)
        cos-theta (m/cos radians)
        {:keys [minimum maximum] :as bbox} (bounding-box p 0 1 nil)
        [^double mnx ^double mny ^double mnz] minimum
        [^double mxx ^double mxy ^double mxz] maximum
        {:keys [mn mx]}
        (->> (for [i [0 1]
                   j [0 1]
                   k [0 1]]
               [i j k])
             (reduce (fn [{:keys [mn mx]} [i j k]]
                       (let [x ^double (+ (* i mxx) (* (- 1 i) mnx))
                             y ^double (+ (* j mxy) (* (- 1 j) mny))
                             z ^double (+ (* k mxz) (* (- 1 k) mnz))
                             newx (+ (* cos-theta x)
                                     (* sin-theta z))
                             newz (+ (* (- sin-theta) x)
                                     (* cos-theta z))
                             tester [newx y newz]]
                         {:mn (mapv min mn tester)
                          :mx (mapv min mx tester)}))
                     {:mn [##Inf ##Inf ##Inf]
                      :mx [##-Inf ##-Inf ##-Inf]}))]
    (->RotateY p
               sin-theta
               cos-theta
               bbox
               (aabb mn mx))))