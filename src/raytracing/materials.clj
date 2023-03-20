(ns raytracing.materials
  (:require [raytracing.base :refer :all]
            [raytracing.texture :refer :all]
            [raytracing.ray :refer :all]
            [clojure.math :as m])
  (:gen-class))

(defprotocol Material
  (scatter [mat r-in rec])
  (emitted [this u v p]))

(defrecord Lambertian [albedo]
  Material
  (scatter [{:keys [albedo_ fuzz] :as mat}
            {:keys [time_] :as r-in}
            {:keys [normal u v p] :as rec}]
    (let [scatter-direction (v+ normal
                                (random-unit-vector))]
      {:ok          true
       :scattered   (ray p scatter-direction time_)
       :attenuation (value albedo u v p)}))
  (emitted [this u v p] [0.0 0.0 0.0]))

(defn lambertian [a]
  (if (satisfies? Texture a)
    (->Lambertian a)
    (->Lambertian (solid-color a))))

(defrecord Metal [albedo fuzz]
  Material
  (scatter [{:keys [albedo fuzz] :as mat}
            {:keys [orig dir time_] :as r-in}
            {:keys [normal p] :as rec}]
    (let [reflected (reflect (unit-vector dir) normal)
          scattered (ray p (v+ reflected (v* (random-in-unit-sphere) fuzz)) time_)
          ok (pos? ^double (dot (:dir scattered) normal))]
      {:ok ok :attenuation albedo :scattered scattered}))
  (emitted [this u v p] [0.0 0.0 0.0]))

(defn metal [color f]
  (->Metal color (if (> 1 ^double f) f 1)))

(defrecord Dielectric [ir]
  Material
  (scatter [{:keys [ir] :as mat}
            {:keys [orig dir time_] :as r-in}
            {:keys [p front-face normal] :as rec}]
    (let [attenuation [1.0 1.0 1.0]
          refraction-ratio (if front-face (/ 1.0 ^double ir) ir)
          unit-direction (unit-vector dir)
          cos-theta (min ^double (dot (map - unit-direction) normal) 1.0)
          sin-theta (m/sqrt (- 1.0 (m/pow cos-theta 2)))
          cannot-refract (> ^double (* ^double refraction-ratio ^double sin-theta) 1.0)
          direction (if (or cannot-refract (> ^double (reflectance cos-theta refraction-ratio) ^double (rand)))
                      (reflect unit-direction normal)
                      (refract unit-direction normal refraction-ratio))]
      {:ok          true
       :attenuation attenuation
       :scattered   (ray p direction time_)}))
  (emitted [this u v p] [0.0 0.0 0.0]))

(defrecord DiffuseLight [emit]
  Material
  (scatter [{:keys [albedo_ fuzz] :as mat}
            {:keys [time_] :as r-in}
            {:keys [normal u v p] :as rec}]
    (let [scatter-direction (v+ normal
                                (random-unit-vector))]
      {:ok          false
       :scattered   (ray p scatter-direction time_)
       :attenuation (value emit u v p)}))
  (emitted [this u v p]
    (value emit u v p)))

(defn diffuse-light [a]
  (if (satisfies? Texture a)
    (->DiffuseLight a)
    (->DiffuseLight (solid-color a))))

(defrecord Isotropic [albedo]
  Material
  (scatter [this {:keys [p time_]} {:keys [u v p]}]
    {:ok          true
     :scattered   (ray p (random-in-unit-sphere) time_)
     :attenuation (value albedo u v p)})
  (emitted [this u v p] [0.0 0.0 0.0]))

(defn isotropic [c]
  (if (satisfies? Texture c)
    (->Isotropic c)
    (->Isotropic (solid-color c))))