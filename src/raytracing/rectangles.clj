(ns raytracing.rectangles
  (:require
    [raytracing.base :refer :all])
  (:gen-class))

(set! *unchecked-math* :warn-on-boxed)

(defrecord XYRect [mp
                   ^double x0
                   ^double x1
                   ^double y0
                   ^double y1
                   ^long k]
  Hittable
  (hit [this
        {:keys [orig dir] :as r}
        t-min
        t-max
        rec]
    (let [[^double o-x ^double o-y ^double o-z] orig
          [^double d-x ^double d-y ^double d-z] dir
          t ^double (/ ^double (- k o-z) d-z)]
      (when-not (or (< t ^double t-min)
                    (> t ^double t-max))
        (let [x ^double (+ o-x ^double (* t d-x))
              y ^double (+ o-y ^double (* t d-y))]
          (when-not (or (< x ^double x0)
                        (> x ^double x1)
                        (< y ^double y0)
                        (> y ^double y1))
            (let [u ^double (/ ^double (- x x0)
                               ^double (- x1 x0))
                  v ^double (/ ^double (- y y0)
                               ^double (- y1 y0))
                  outward-normal [0.0 0.0 1.0]
                  {:keys [front-face normal]} (set-face-normal r outward-normal)]
              (->HitRecord (ray-at r t) normal mp t u v front-face)))))))
  (bounding-box [this time0 time1 output-box]
    (aabb [x0 y0 (- k 0.0001)]
          [x1 y1 (+ k 0.0001)])))

(defn xy-rect [x0 x1 y0 y1 k mat]
  (->XYRect mat x0 x1 y0 y1 k))

(defrecord XZRect [mp
                   ^double x0
                   ^double x1
                   ^double z0
                   ^double z1
                   ^long k]
  Hittable
  (hit [this
        {:keys [orig dir] :as r}
        t-min
        t-max
        rec]
    (let [[^double o-x ^double o-y ^double o-z] orig
          [^double d-x ^double d-y ^double d-z] dir
          t ^double (/ ^double (- k o-y) d-y)]
      (when-not (or (< t ^double t-min)
                    (> t ^double t-max))
        (let [x ^double (+ o-x ^double (* t d-x))
              z ^double (+ o-z ^double (* t d-z))]
          (when-not (or (< x ^double x0)
                        (> x ^double x1)
                        (< z ^double z0)
                        (> z ^double z1))
            (let [u ^double (/ ^double (- x x0)
                               ^double (- x1 x0))
                  v ^double (/ ^double (- z z0)
                               ^double (- z1 z0))
                  outward-normal [0.0 1.0 0.0]
                  {:keys [front-face normal]} (set-face-normal r outward-normal)]
              (->HitRecord (ray-at r t) normal mp t u v front-face)))))))
  (bounding-box [this time0 time1 output-box]
    (aabb [x0 (- k 0.0001) z0]
          [x1 (+ k 0.0001) z1])))

(defn xz-rect [x0 x1 z0 z1 k mat]
  (->XZRect mat x0 x1 z0 z1 k))

(defrecord YZRect [mp
                   ^double y0
                   ^double y1
                   ^double z0
                   ^double z1
                   ^long k]
  Hittable
  (hit [this
        {:keys [orig dir] :as r}
        t-min
        t-max
        rec]
    (let [[^double o-x ^double o-y ^double o-z] orig
          [^double d-x ^double d-y ^double d-z] dir
          t ^double (/ ^double (- k o-x) d-x)]
      (when-not (or (< t ^double t-min)
                    (> t ^double t-max))
        (let [y ^double (+ o-y ^double (* t d-y))
              z ^double (+ o-z ^double (* t d-z))]
          (when-not (or (< y ^double y0)
                        (> y ^double y1)
                        (< z ^double z0)
                        (> z ^double z1))
            (let [u ^double (/ ^double (- y y0)
                               ^double (- y1 y0))
                  v ^double (/ ^double (- z z0)
                               ^double (- z1 z0))
                  outward-normal [1.0 0.0 0.0]
                  {:keys [front-face normal]} (set-face-normal r outward-normal)]
              (->HitRecord (ray-at r t) normal mp t u v front-face)))))))
  (bounding-box [this time0 time1 output-box]
    (aabb [(- k 0.0001) y0 z0]
          [(+ k 0.0001) y1 z1])))

(defn yz-rect [y0 y1 z0 z1 k mat]
  (->YZRect mat y0 y1 z0 z1 k))