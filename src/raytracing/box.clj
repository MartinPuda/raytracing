(ns raytracing.box
  (:require [raytracing.base :refer :all]
            [raytracing.rectangles :refer :all])
  (:gen-class))

(defrecord Box [box-min box-max sides]
  Hittable
  (hit [this r t-min t-max rec]
    (hit (->HittableList sides) r t-min t-max rec))
  (bounding-box [this time0 time1 output-box]
    (aabb box-min box-max)))

(defn box [p0 p1 ptr]
  (let [[^long x0 ^long y0 ^long z0] p0
        [^long x1 ^long y1 ^long z1] p1
        sides [(xy-rect x0 x1 y0 y1 z1 ptr)
               (xy-rect x0 x1 y0 y1 z0 ptr)
               (xz-rect x0 x1 z0 z1 y1 ptr)
               (xz-rect x0 x1 z0 z1 y0 ptr)
               (yz-rect y0 y1 z0 z1 x1 ptr)
               (yz-rect y0 y1 z0 z1 x0 ptr)]]
    (->Box p0 p1 sides)))