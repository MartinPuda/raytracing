(ns raytracing.camera
  (:require
    [raytracing.base :refer :all]
    [clojure.math :as m])
  (:gen-class))

(defn camera [lookfrom lookat vup vfov aspect-ratio
              aperture focus-dist time0 time1]
  (let [theta (m/to-radians vfov)
        h (m/tan (* theta 0.5))
        viewport-height (* h 2.0)
        viewport-width (* ^double aspect-ratio viewport-height)
        w (unit-vector (v- lookfrom lookat))
        u (unit-vector (cross vup w))
        v (cross w u)
        origin lookfrom
        horizontal (v* u (* ^double focus-dist viewport-width))
        vertical (v* v (* ^double focus-dist viewport-height))
        lower-left-corner (v- origin
                              (vd horizontal 2)
                              (vd vertical 2)
                              (v* w focus-dist))
        lens-radius (/ ^double aperture 2)]
    {:origin            origin
     :lower-left-corner lower-left-corner
     :horizontal        horizontal
     :vertical          vertical
     :u                 u
     :v                 v
     :w                 w
     :lens-radius       lens-radius
     :time0             time0
     :time1             time1}))