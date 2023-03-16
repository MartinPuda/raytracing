(ns raytracing.base
  (:require [clojure.java.io :as io]
            [clojure.math :as m])
  (:import (javax.imageio ImageIO)
           (java.awt.image DataBufferByte)
           (java.net URL))
  (:gen-class))

;zacit s 7

(set! *unchecked-math* :warn-on-boxed)

;;;; Vector functions

(defn dot [[^double a ^double b ^double c]
           [^double d ^double e ^double f]]
  (+ (* a d) (* b e) (* c f)))

(defn v* [[^double x ^double y ^double z] ^double k]
  [(* x k)
   (* y k)
   (* z k)])

(defn v+
  ([[^double a ^double b ^double c]
    [^double d ^double e ^double f]] [(+ a d) (+ b e) (+ c f)])
  ([v1 v2 & vs] (reduce v+ (v+ v1 v2) vs)))

(defn v-
  ([[^double a ^double b ^double c]
    [^double d ^double e ^double f]] [(- a d) (- b e) (- c f)])
  ([v1 v2 & vs] (reduce v- (v- v1 v2) vs)))

(defn vd [v ^double k]
  (v* v (/ 1 k)))

(defn length-squared [[^double x ^double y ^double z]]
  (+ (* x x) (* y y) (* z z)))

(defn length [[^double x ^double y ^double z]]
  (m/sqrt (+ (* x x) (* y y) (* z z))))

(defn unit-vector [v]
  (vd v (length v)))

(defn cross [[^double u0 ^double u1 ^double u2]
             [^double v0 ^double v1 ^double v2]]
  [(- (* u1 v2) (* u2 v1))
   (- (* u2 v0) (* u0 v2))
   (- (* u0 v1) (* u1 v0))])

;;;; Rendering functions

(defprotocol Hittable
  (hit [this
        r
        t-min
        t-max
        rec])
  (center [this time_])
  (bounding-box [this time0 time1 output-box]))

(defrecord Aabb [minimum maximum]
  Hittable
  (hit [this {:keys [orig dir]} t-min t-max rec]
    (reduce (fn [{:keys [t-min t-max]} a]
              (let [invD ^double (/ 1.0 ^double (dir a))
                    t0 ^double (if (neg? invD)
                                 (- ^double (maximum a) (* ^double (orig a) invD))
                                 (- ^double (minimum a) (* ^double (orig a) invD)))
                    t1 ^double (if (neg? invD)
                                 (- ^double (minimum a) (* ^double (orig a) invD))
                                 (- ^double (maximum a) (* ^double (orig a) invD)))
                    t-min (if (> ^double t0 ^double t-min) t0 t-min)
                    t-max (if (< ^double t1 ^double t-max) t1 t-max)]
                (if (<= ^double t-max ^double t-min)
                  (reduced false)
                  {:t-min ^double t-min :t-max ^double t-max})))
            {:t-min t-min :t-max t-max}
            (range 0 3)))
  (center [this time_]))

(defn aabb
  ([a b] (->Aabb a b)))

(defn surrounding-box [box0 box1]
  (let [small (let [v1 (:minimum box0)
                    v2 (:minimum box1)]
                (mapv #(min ^double %1 ^double %2) v1 v2))
        big (let [v1 (:maximum box0)
                  v2 (:maximum box1)]
              (mapv #(max ^double %1 ^double %2) v1 v2))]
    (aabb small big)))

(defrecord HittableList [objects]
  Hittable
  (hit [this {:keys [orig dir] :as r} t-min t-max rec]
    (-> (reduce (fn [hr hittable]
                  (let [rec (hit hittable r t-min (:closest-so-far hr) rec)]
                    (cond-> hr
                            rec (assoc :closest-so-far (:t rec)
                                       :record rec))))
                {:record         nil
                 :closest-so-far t-max}
                objects)
        :record))
  (bounding-box [this time0 time1 output-box]
    (when (seq objects)
      (reduce (fn [{:keys [temp-box first-box output-box] :as boxes} object]
                (if (bounding-box object time0 time1 temp-box)
                  (-> boxes
                      (assoc :output-box (if first-box temp-box (surrounding-box output-box temp-box)))
                      (assoc :first-box false))
                  (reduced false)))
              {:temp-box nil :first-box true :output-box output-box}
              objects))))

(defrecord BvhNode [left right box]
  Hittable
  (hit [this r t-min t-max rec]
    (when (hit box r t-min t-max rec)
      (let [hit-left (hit this r t-min t-max rec)
            hit-right (hit this r t-min (if hit-left (:t rec) t-max) rec)]
        (or hit-left hit-right))))
  (bounding-box [this time0 time1 output-box-ptr]
    box))

(defn random-double
  ([] (rand))
  ([^double mn ^double mx] (+ mn (* (- mx mn) ^double (rand)))))

(defn random-int [min_ max_]
  (int (random-double min_ (inc ^double max_))))

(defn box-compare [a b axis]
  (let [box-a (bounding-box a 0 0 nil)
        box-b (bounding-box b 0 0 nil)]
    ;(if (or (not box-a) (not box-b))
    ;  (throw (Exception. "No bounding box in bvh_node constructor.\n")))
    (< ^double ((:minimum box-a) axis)
       ^double ((:minimum box-b) axis))))

(defn box-x-compare [a b]
  (box-compare a b 0))

(defn box-y-compare [a b]
  (box-compare a b 1))

(defn box-z-compare [a b]
  (box-compare a b 2))

(defn bvhnode
  ([list_ time0 time1] (bvhnode (:objects list_) 0 (count (:objects list_)) time0 time1))
  ([src-objects start end time0 time1]
   (let [objects src-objects
         axis (random-int 0 2)
         comparator_ (cond (zero? ^double axis) box-x-compare
                           (== ^double axis 1) box-y-compare
                           :else box-z-compare)
         object-span ^double (- ^double end ^double start)
         [left right] (cond (== ^double object-span 1)
                            [(objects start) (objects start)]
                            (== object-span 2)
                            (if (comparator_ (objects start)
                                             (objects (inc ^double start)))
                              [(objects start) (objects (inc ^double start))]
                              [(objects (inc ^double start)) (objects start)])
                            :else (let [sorted-obj (sort-by comparator_ (objects start))

                                        mid (+ ^double start ^double (/ object-span 2))]
                                    [(bvhnode sorted-obj start mid time0 time1)
                                     (bvhnode sorted-obj mid end time0 time1)]))
         box-left (bounding-box left time0 time1 nil)
         box-right (bounding-box right time0 time1 nil)]
     ;(if (or (not box-left)
     ;        (not box-right))
     ;  (throw (Exception. "No bounding box in bvh_node constructor.\n"))
       (->BvhNode left right (surrounding-box box-left box-right)))))

(defprotocol Texture
  (value [this u v p]))

(defrecord SolidColor [color-value]
  Texture
  (value [this u v p] color-value))

(defn solid-color
  ([c] (->SolidColor c))
  ([red green blue] (solid-color [red green blue])))

(defrecord CheckerTexture [odd even]
  Texture
  (value [this u v p]
    (let [sines (transduce (map #(m/sin (* 10 ^double %)))
                           * p)]
      (if (neg? ^double sines)
        (value odd u v p)
        (value even u v p)))))

(defn checker-texture
  ([c1 c2] (if (satisfies? Texture c1)
             (->CheckerTexture c1 c2)
             (->CheckerTexture (solid-color c1)
                               (solid-color c2)))))

(defn ray-at [{:keys [orig dir]} t]
  (v+ orig (v* dir t)))

(defrecord HitRecord [p normal mat-ptr t u v front-face])

(defn set-face-normal [{:keys [orig dir] :as r} outward-normal]
  (let [front-face (neg? ^double (dot dir outward-normal))]
    {:front-face front-face
     :normal     (if front-face outward-normal (map - outward-normal))}))

(defn random-vec3
  ([] [(rand) (rand) (rand)])
  ([mn mx] [(random-double mn mx)
            (random-double mn mx)
            (random-double mn mx)]))

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

(defrecord Ray [orig dir time_])

(defprotocol Material
  (scatter [mat r-in rec])
  (emitted [this u v p]))

(defn ray
  ([orig dir] (->Ray orig dir 0.0))
  ([orig dir time_] (->Ray orig dir time_)))

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

(defn get-sphere-uv [this [x y z] u v]
  (let [theta (m/acos (- ^double y))
        phi (+ (m/atan2 (- ^double z) ^double x)
               m/PI)]
    {:u (/ phi (* 2 m/PI))
     :v (/ theta m/PI)}))

(defrecord MovingSphere [center0 center1
                         ^double time0
                         ^double time1
                         ^double radius
                         mat-ptr]
  Hittable
  (hit [{:keys [radius mat-ptr] :as sphere}
        {:keys [orig dir time_] :as r}
        t-min
        t-max
        rec]
    (let [oc (v- orig (center sphere time_))
          a (length-squared dir)
          half-b ^double (dot oc dir)
          c (- ^double (length-squared oc) ^double (m/pow radius 2))
          discriminant (- ^double (m/pow half-b 2) ^double (* ^double a ^double c))]
      (when (pos? ^double discriminant)
        (let [sqrtd (m/sqrt discriminant)
              root (/ ^double (- (- ^double half-b) ^double sqrtd) ^double a)]
          (if (> t-max root t-min)
            (let [outward-normal (vd (v- (ray-at r root) (center sphere time_)) radius)
                  {:keys [u v]} (get-sphere-uv sphere outward-normal (:u rec) (:v rec))
                  {:keys [front-face normal]} (set-face-normal r outward-normal)]
              (->HitRecord (ray-at r root) normal mat-ptr root u v front-face))
            (let [root (/ ^double (+ ^double (- ^double half-b) root) ^double a)]
              (when (> t-max root t-min)
                (let [outward-normal (vd (v- (ray-at r root) (center sphere time_)) radius)
                      {:keys [u v]} (get-sphere-uv sphere outward-normal (:u rec) (:v rec))
                      {:keys [front-face normal]} (set-face-normal r outward-normal)]
                  (->HitRecord (ray-at r root) normal mat-ptr root u v front-face)))))))))
  (center [this time_]
    (v+ center0
        (v* (v- center1 center0)
            (/ (- ^double time_ ^double time0)
               (- ^double time1 ^double time0))))))

(defn moving-sphere [center0 center1 time0 time1 radius mat_ptr]
  (->MovingSphere center0 center1 time0 time1 radius mat_ptr))

(defrecord Sphere [center radius mat-ptr]
  Hittable
  (hit [{:keys [center radius mat-ptr] :as sphere}
        {:keys [orig dir] :as r}
        t-min t-max rec]
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
                  {:keys [u v]} (get-sphere-uv sphere outward-normal (:u rec) (:v rec))
                  {:keys [front-face normal]} (set-face-normal r outward-normal)]
              (->HitRecord (ray-at r root) normal mat-ptr root u v front-face))
            (let [root (/ ^double (+ ^double (- ^double half-b) root) ^double a)]
              (when (> t-max root t-min)
                (let [outward-normal (vd (v- (ray-at r root) center) radius)
                      {:keys [u v]} (get-sphere-uv sphere outward-normal (:u rec) (:v rec))
                      {:keys [front-face normal]} (set-face-normal r outward-normal)]
                  (->HitRecord (ray-at r root) normal mat-ptr root u v front-face)))))))))
  (center [this time_] center))

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

(defn lambertian
  ([a] (->Lambertian (if (satisfies? Texture a)
                       a
                       (solid-color a)))))

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
      {:ok true
       :attenuation attenuation
       :scattered (ray p direction time_)}))
  (emitted [this u v p] [0.0 0.0 0.0]))

(defn ray-color [{:keys [_ dir] :as r} background world ^double depth]
  (if (>= 0 depth)
    [0.0 0.0 0.0]
    (if-let [{:keys [mat-ptr u v p] :as rec}
             (hit (->HittableList world) r 0.001 ##Inf {})]
      (let [emitted (emitted mat-ptr u v p)
            {:keys [ok attenuation scattered]} (scatter mat-ptr r rec)]
        (if ok
          (map * attenuation
               (ray-color scattered background world (dec depth)))
          emitted))
      background)))

;(let [[x y z] (unit-vector dir)
;      t (* 0.5 (inc ^double y))]
;  (v+ (v* [1.0 1.0 1.0] (- 1.0 t))
;      (v* [0.5 0.7 1.0] t))))))

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

(defn get-ray [{:keys [u v horizontal vertical
                       lower-left-corner
                       origin lens-radius time0 time1]}
               ^double s ^double t]
  (let [[x y z] (v* (random-in-unit-disk) lens-radius)
        offset (v+ (v* u x) (v* v y))]
    (ray (v+ origin offset)
         (v- (v+ lower-left-corner
                 (v* horizontal s)
                 (v* vertical t))
             origin
             offset)
         (random-double time0 time1))))

(defn v-dist [v1 v2]
  (m/sqrt (length (v- v1 v2))))

(defn z-fix [objects]
  (->> objects
       (sort-by #(v-dist [13 2 3] (:center %)) <)
       vec))

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

(defrecord NoiseTexture [perlin-noise
                         ^long scale]
  Texture
  (value [this u v p]
    (v* [1 1 1] (* 0.5
                   (inc ^double (m/sin ^double (+ (* scale ^double (p 2))
                                                  (* 10 ^double (turb perlin-noise p 7)))))))))

(defn noise-texture
  ([] (->NoiseTexture (perlin) 1.0))
  ([sc] (->NoiseTexture (perlin) sc)))

(defn clamp [^double x ^double mn ^double mx]
  (cond (> mn x) mn
        (> x mx) mx
        :else x))

(defrecord Sphere [center radius mat-ptr]
  Hittable
  (hit [{:keys [center radius mat-ptr] :as sphere}
        {:keys [orig dir] :as r}
        t-min t-max rec]
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
                  {:keys [u v]} (get-sphere-uv sphere outward-normal (:u rec) (:v rec))
                  {:keys [front-face normal]} (set-face-normal r outward-normal)]
              (->HitRecord (ray-at r root) normal mat-ptr root u v front-face))
            (let [root (/ ^double (+ ^double (- ^double half-b) root) ^double a)]
              (when (> t-max root t-min)
                (let [outward-normal (vd (v- (ray-at r root) center) radius)
                      {:keys [u v]} (get-sphere-uv sphere outward-normal (:u rec) (:v rec))
                      {:keys [front-face normal]} (set-face-normal r outward-normal)]
                  (->HitRecord (ray-at r root) normal mat-ptr root u v front-face)))))))))
  (center [this time_] center))

