(ns raytracing.core
  (:require [clojure.math :as m]
            [clojure.math.combinatorics :as c])
  (:import (javax.swing JFrame JPanel)
           (java.awt Dimension BorderLayout Color)
           (java.awt.image BufferedImage))
  (:gen-class))

(set! *unchecked-math* :warn-on-boxed)

;;;; Vector functions

(defn dot [v1 v2]
  (apply + (map * v1 v2)))

(defn v* [[^double x ^double y ^double z] ^double k]
  [(* x k)
   (* y k)
   (* z k)])

(defn v+ [& vs]
  (apply map + vs))

(defn v- [& vs]
  (apply map - vs))

(defn vd [v ^double k]
  (v* v (/ 1 k)))

(defn length-squared [[^double x ^double y ^double z]]
  (+ (* x x) (* y y) (* z z)))

(defn length [v]
  (m/sqrt (length-squared v)))

(defn unit-vector [v]
  (vd v (length v)))

(defn cross [[^double u0 ^double u1 ^double u2]
             [^double v0 ^double v1 ^double v2]]
  [(- (* u1 v2) (* u2 v1))
   (- (* u2 v0) (* u0 v2))
   (- (* u0 v1) (* u1 v0))])

;;;; Rendering functions

(defn ray-at [{:keys [orig dir]} t]
  (v+ orig (v* dir t)))

(defrecord HitRecord [p normal mat-ptr t front-face])
(defrecord Ray [orig dir])

(defn set-face-normal [{:keys [orig dir] :as r} outward-normal]
  (let [front-face (neg? ^double (dot dir outward-normal))]
    {:front-face front-face
     :normal     (if front-face outward-normal (map - outward-normal))}))

(defn random-double
  ([] (rand))
  ([^double mn ^double mx] (+ mn (* (- mx mn) ^double (rand)))))

(defn random-vec3
  ([] (vec (repeatedly 3 #(rand))))
  ([mn mx] (vec (repeatedly 3 #(random-double mn mx)))))

(defn random-in-unit-sphere []
  (reduce
    (fn [_ v] (if (>= ^double (length-squared v) 1)
                (reduced v) _))
    (repeatedly #(random-vec3 -1 1))))

(defn random-in-unit-disk []
  (reduce
    (fn [_ v] (if (< ^double (length-squared v) 1)
                (reduced v) _))
    (repeatedly #(vector (random-double -1 1)
                         (random-double -1 1)
                         0))))

(defn random-unit-vector []
  (unit-vector (random-in-unit-sphere)))

(defn reflect [v n]
  (v- v (v* n (* 2 ^double (dot v n)))))

(defn refract [uv n etai-over-etat]
  (let [cos-theta (min ^double (dot (map - uv) n) 1.0)
        r-out-perp (v* (v+ uv (v* n cos-theta))
                       etai-over-etat)
        r-out-parallel
        (v* n
            (- (m/sqrt (abs (- 1.0 ^double (length-squared r-out-perp))))))]
    (v+ r-out-perp r-out-parallel)))

(defn near-zero [v3]
  (let [s 1e-8]
    (every? #(> s ^double (abs ^double %)) v3)))

(defn reflectance [^double cosine ^double ref-idx]
  (let [r0 (m/pow
             (/ (- 1.0 ref-idx)
                (+ 1.0 ref-idx)) 2)]
    (+ r0 (* (- 1.0 r0)
             (m/pow (- 1 cosine) 5)))))

(defrecord Sphere [center radius mat-ptr])
(defrecord Ray [orig dir])

(defprotocol Material
  (scatter [mat r-in rec]))

(defrecord Lambertian [albedo]
  Material
  (scatter [{:keys [albedo fuzz] :as mat} r-in {:keys [normal p] :as rec}]
    (let [scatter-direction (v+ normal
                                (random-unit-vector))]
      {:ok          true
       :scattered   (->Ray p (if (near-zero scatter-direction)
                               normal
                               scatter-direction))
       :attenuation albedo})))

(defrecord Metal [albedo fuzz]
  Material
  (scatter [{:keys [albedo fuzz] :as mat}
            {:keys [orig dir] :as r-in}
            {:keys [normal p] :as rec}]
    (let [reflected (reflect (unit-vector dir) normal)
          scattered (->Ray p (v+ reflected (v* (random-in-unit-sphere) fuzz)))
          ok (pos? ^double (dot (:dir scattered) normal))]
      {:ok ok :attenuation albedo :scattered scattered})))

(defn metal [color f]
  (Metal. color (if (> 1 ^double f) f 1)))

(defrecord Dielectric [ir]
  Material
  (scatter [{:keys [ir] :as mat}
            {:keys [orig dir] :as r-in}
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
      {:ok true :attenuation attenuation :scattered (->Ray p direction)})))

(defn hit-sphere [{:keys [center radius mat-ptr] :as sphere}
                  {:keys [orig dir] :as r} t-min t-max]
  (let [oc (v- orig center)
        a (length-squared dir)
        half-b ^double (dot oc dir)
        c (- ^double (length-squared oc) ^double (m/pow radius 2))
        discriminant (- ^double (m/pow half-b 2) ^double (* ^double a ^double c))]
    (when (pos? ^double discriminant)
      (let [sqrtd (m/sqrt discriminant)
            root (/ ^double (- (- ^double half-b) ^double sqrtd) ^double a)]
        (if (> t-max root t-min)
          (let [outward-normal (vd (v- (ray-at r root) center) radius)
                {:keys [front-face normal]} (set-face-normal r outward-normal)]
            (->HitRecord (ray-at r root) normal mat-ptr root front-face))
          (let [root (/ ^double (+ ^double (- ^double half-b) root) ^double a)]
            (when (> t-max root t-min)
              (let [outward-normal (vd (v- (ray-at r root) center) radius)
                    {:keys [front-face normal]} (set-face-normal r outward-normal)]
                (->HitRecord (ray-at r root) normal mat-ptr root front-face)))))))))

(defn hittable-list-hit [hl {:keys [orig dir] :as r} t-min t-max]
  (let [record (volatile! nil)
        closest-so-far (volatile! t-max)]
    (doseq [object hl]
      (when-let [rec (hit-sphere object r t-min @closest-so-far)]
        (vreset! closest-so-far (:t rec))
        (vreset! record rec)))
    @record))

;(defn random-in-hemisphere [normal]
;  (let [in-unit-sphere (random-in-unit-sphere)]
;    (if (> (dot in-unit-sphere normal) 0.0)
;      in-unit-sphere
;      (- in-unit-sphere))))

(defn ray-color [{:keys [_ dir] :as r} world ^double depth]
  (if (>= 0 depth)
    [0 0 0]
    (if-let [{:keys [mat-ptr] :as rec} (hittable-list-hit world r 0.001 ##Inf)]
      (let [{:keys [ok attenuation scattered]} (scatter mat-ptr r rec)]
        (if ok
          (map * attenuation (ray-color scattered world (dec depth)))
          [0 0 0]))
      (let [[x y z] (unit-vector dir)
            t (* 0.5 (inc ^double y))]
        (v+ (v* [1.0 1.0 1.0] (- 1.0 t))
            (v* [0.5 0.7 1.0] t))))))

(defn camera [lookfrom lookat vup vfov aspect-ratio aperture focus-dist]
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
        lower-left-corner (v- origin (vd horizontal 2) (vd vertical 2) (v* w focus-dist))
        lens-radius (/ ^long aperture 2)]
    {:origin            origin
     :lower-left-corner lower-left-corner
     :horizontal        horizontal
     :vertical          vertical
     :u                 u
     :v                 v
     :w                 w
     :lens-radius       lens-radius}))

(defn camera-old [lookfrom lookat vup vfov
                  aspect-ratio]
  (let [theta (m/to-radians vfov)
        h (m/tan (/ theta 2))
        viewport-height (* 2.0 h)
        viewport-width (* ^double aspect-ratio viewport-height)
        w (unit-vector (v- lookfrom lookat))
        u (unit-vector (cross vup w))
        v (cross w u)
        origin lookfrom
        horizontal (v* u viewport-width)
        vertical (v* v viewport-height)
        lower-left-corner (v- origin
                              (vd horizontal 2)
                              (vd vertical 2)
                              w)]
    {:origin     origin :lower-left-corner lower-left-corner
     :horizontal horizontal :vertical vertical}))

(defn get-ray [{:keys [u v horizontal vertical
                       lower-left-corner
                       origin lens-radius]}
               ^double s ^double t]
  (let [[x y z] (v* (random-in-unit-disk) lens-radius)
        offset (v+ (v* u x) (v* v y))]
    (Ray. (v+ origin offset)
          (v- (v+ lower-left-corner
                  (v* horizontal s)
                  (v* vertical t))
              origin
              offset))))

(defn get-ray-old [{:keys [origin lower-left-corner horizontal vertical]} s t]
  (Ray. origin
        (v- (v+ lower-left-corner
                (v* horizontal s)
                (v* vertical t))
            origin)))

(defn clamp [^double x ^double mn ^double mx]
  (cond (> mn x) mn
        (> x mx) mx
        :else x))

(defn display-image [img w h]
  (let [ppm-panel (doto (proxy [JPanel] []
                          (paintComponent [graphics]
                            (.drawImage graphics img 0 0 nil)))
                    (.setLayout (BorderLayout.)))
        frame (doto (JFrame. "Ray Tracing")
                (.setPreferredSize (Dimension. (+ ^long w 20) (+ ^long h 75)))
                (.add ppm-panel))]
    (doto frame
      (.pack)
      (.setVisible true))))

(defn random-scene []
  (into (->> (c/cartesian-product (range -11 11)
                                  (range -11 11))
             (map (fn [[a b]] (Sphere. [(+ ^long a (* 0.9 ^double (rand)))
                                        0.2
                                        (+ ^long b (* 0.9 ^double (rand)))]
                                       0.2
                                       (let [choose-mat ^double (rand)]
                                         (cond (< ^double choose-mat 0.8) (Lambertian. (map * (random-vec3) (random-vec3)))
                                               (< ^double choose-mat 0.95) (Metal. (random-vec3 0.5 1)
                                                                                   (random-double 0 0.5))
                                               :else (Dielectric. 1.5)))))))
        [(Sphere. [0 -1000 0] 1000 (Lambertian. [0.5 0.5 0.5]))
         (Sphere. [0 1 0] 1.0 (Dielectric. 1.5))
         (Sphere. [-4 1 0] 1.0 (Lambertian. [0.4 0.2 0.1]))
         (Sphere. [4 1 0] 1.0 (Metal. [0.7 0.6 0.5] 0.0))]))

(defn -main [& {:keys [samples new-camera]
                :or   {samples 20 new-camera false}}]
  (let [aspect-ratio (/ 16.0 9.0)
        image-width 400
        image-height (/ image-width aspect-ratio)
        image-width-dec (dec image-width)
        image-height-dec (dec image-height)
        samples-per-pixel samples
        max-depth 50
        material-ground (Lambertian. [0.8 0.8 0.0])
        material-center (metal [0.8 0.6 0.2] 0.0)
        material-left (Lambertian. [0.1 0.2 0.5])
        material-right (Lambertian. [0.1 0.2 0.5])          ;(Dielectric. 1.5);
        world                                               ;  (random-scene)
        [(Sphere. [0.0 -100.5 -1.0] 100.0 material-ground)
         (Sphere. [0.0 0.0 -1.0] 0.5 material-center)
         (Sphere. [-1.0 0.0 -1.0] 0.5 material-left)
         (Sphere. [1.0 0.0 -1.0] 0.5 material-right)]
        cam (if new-camera
              (let [lookfrom [3 3 2]
                    lookat [0 0 -1]
                    vup [0 1 0]
                    dist-to-focus (length (v- lookfrom lookat))
                    aperture 2.0]
                (camera lookfrom lookat vup 20 aspect-ratio aperture dist-to-focus))
              (camera-old [0 3 2]
                          [0 0 -1]
                          [0 1 0]
                          30
                          aspect-ratio))
        get-ray-version (if new-camera get-ray get-ray-old)
        buffered-image (BufferedImage. 500 500 BufferedImage/TYPE_INT_RGB)]
    (dotimes [y image-height]
      (println "Rendering line " y)
      (dotimes [x image-width]
        (let [pixel-color
              (reduce v+ [0 0 0]
                      (repeatedly
                        samples-per-pixel
                        #(ray-color
                           (get-ray-version cam
                                            (/ ^double (+ ^long x ^double (rand))
                                               image-width-dec)
                                            (/ ^double (+ ^long y ^double (rand))
                                               image-height-dec))
                           world max-depth)))]
          (let [scale (/ 1.0 ^long samples-per-pixel)
                [r g b] (->> (v* pixel-color scale)
                             (map m/sqrt)
                             (map #(clamp % 0.0 0.999))
                             (map #(* ^double % 255)))]
            (.setRGB buffered-image x (- 256 y)
                     (.getRGB (Color. ^int r
                                      ^int g
                                      ^int b)))))))
    (display-image buffered-image image-width image-height)))