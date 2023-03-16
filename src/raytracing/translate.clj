(ns raytracing.translate
  (:require [raytracing.base :refer :all])
  (:gen-class))

(defrecord Translate [ptr offset]
  Hittable
  (hit [this {:keys [orig dir time_]} t-min t-max rec]
    (let [moved-r (ray (v- orig offset) dir time_)]
      (when-let [hit-rec (hit ptr moved-r t-min t-max rec)]
        (let [{:keys [front-face normal]} (set-face-normal moved-r (:normal hit-rec))]
          (-> hit-rec
              (update :p v+ offset)
              (assoc :front-face front-face
                     :normal normal))))))
  (bounding-box [this time0 time1 {:keys [minimum maximum] :as output-box}]
    (when (bounding-box ptr time0 time1 output-box)
      (aabb (v+ minimum offset)
            (v+ maximum offset)))))

(defn translate [p displacement]
  (->Translate p displacement))
