(ns raytracing.scenes
  (:require [raytracing.base :refer :all]
            [raytracing.texture :refer :all]
            [raytracing.rectangles :refer :all]
            [raytracing.box :refer :all]
            [raytracing.translate :refer :all]
            [raytracing.rotate :refer :all]
            [clojure.java.io :as io])
  (:gen-class))

(set! *unchecked-math* :warn-on-boxed)

(defn two-spheres []
  (let [checker (checker-texture [0.2 0.3 0.1]
                                 [0.9 0.9 0.9])]
    (z-fix [(->Sphere [0 -10 0] 10 (lambertian checker))
            (->Sphere [0 10 0] 10 (lambertian checker))])))

(defn two-perlin-spheres []
  (let [pertext (noise-texture 4.0)]
    (z-fix [(->Sphere [0 -1000 0] 1000 (lambertian pertext))
            (->Sphere [0 2 0] 2 (lambertian pertext))])))

(defn earth []
  (let [earth-texture (image-texture (io/resource "earthmap.jpg"))
        earth-surface (lambertian earth-texture)]
    [(->Sphere [0 0 0] 2 earth-surface)]))

(defn simple-light []
  (let [pertext (noise-texture 4)
        difflight (diffuse-light [4 4 4])]
    [(->Sphere [0 -1000 0] 1000 (lambertian pertext))
     (->Sphere [0 2 0] 2 (lambertian pertext))
     (xy-rect 3 5 1 3 -2 difflight)]))

(defn random-scene []
  ;(into
  ;  (->> (c/cartesian-product (range -2 2) (range -2 2)) ;(range -11 11) (range -11 11))
  ;       (mapv (fn [[a b]] (let [center [(+ ^long a (* 0.9 ^double (rand)))
  ;                                       0.2
  ;                                       (+ ^long b (* 0.9 ^double (rand)))]]
  ;                           (let [^double choose-mat (rand)]
  ;                             (cond (< choose-mat 0.8)
  ;                                   (->Sphere center 0.2 (lambertian (mapv * (random-vec3) (random-vec3))))
  ;                                   (< choose-mat 0.95)
  ;                                   (->Sphere center 0.2 (->Metal (random-vec3 0.5 1) (random-double 0 0.5)))
  ;                                   :else (->Sphere center 0.2 (->Dielectric 1.5))))))))

  ;(into
  ;  (->> (c/cartesian-product (range -2 2) (range -2 2)) ;(range -11 11) (range -11 11))
  ;
  ;       (mapv (fn [[a b]] (let [center [(+ (* 5 ^double (rand)) ^long a) ;(+ ^long a (* 0.9 ^double (rand)))
  ;                                       0.5
  ;                                       (+ (* 5 ^double (rand)) ^long b)]]
  ;                           (->Sphere center 0.5 (->Metal (random-vec3 0.5 1) (random-double 0 0.0)))))))
  ;  ;(+ ^long b (* 0.9 ^double (rand)))]]
  ;  ; (->Sphere center 0.2 (lambertian (mapv * (random-vec3) (random-vec3))))
  ;  ;   ))))
  ;  ; (->Sphere center 0.5 (->Metal (random-vec3 0.5 1) (random-double 0 0.5)))))))

  (z-fix [
          (->Sphere [4 1 0] 1.0 (->Metal [0.7 0.6 0.5] 0.0))
          (->Sphere [0 1 0] 1.0 (->Metal [0.7 0.6 0.5] 0.0))
          (->Sphere [-4 1 0] 1.0 (->Metal [0.7 0.6 0.5] 0.0))
          ; (->Sphere [0 1 0] 1.0 (->Dielectric 1.5))
          ;  (->Sphere [-4 1 0] 1.0 (lambertian [0.4 0.2 0.1]))

          (->Sphere [0 -1000 0] 1000 (lambertian (checker-texture [0.2 0.3 0.1] [0.9 0.9 0.9])))
          (->Sphere [0 -1000 0] 1000 (lambertian [0.5 0.5 0.5]))]))

(defn cornell-box []
  (let [red (lambertian [0.65 0.05 0.05])
        white (lambertian [0.73 0.73 0.73])
        green (lambertian [0.12 0.45 0.15])
        light (diffuse-light [15 15 15])]
    [(yz-rect 0 555 0 555 555 green)
     (yz-rect 0 555 0 555 0 red)
     (xz-rect 213 343 227 332 554 light)
     (xz-rect 0 555 0 555 0 white)
     (xz-rect 0 555 0 555 555 white)
     (xy-rect 0 555 0 555 555 white)
     (-> (box [0 0 0] [165 330 165] white)
         (rotate-y 15)
         (translate [265 0 295]))
     (-> (box [0 0 0] [165 165 165] white)
         (rotate-y -18)
         (translate [130 0 65]))]))
