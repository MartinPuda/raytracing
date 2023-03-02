(ns raytracing.core
  (:require
    [clojure.math :as m]
    [clojure.math.combinatorics :as c])
  (:import (javax.swing JFrame JPanel)
           (java.awt Dimension BorderLayout Color)
           (java.awt.image BufferedImage)
           (java.util.concurrent CountDownLatch))
  (:gen-class))

;zacit s 6

(set! *unchecked-math* :warn-on-boxed)

;;;; Vector functions

(defn dot [v1 v2]
  (reduce + (mapv * v1 v2)))

(defn v* [[^double x ^double y ^double z] ^double k]
  [(* x k)
   (* y k)
   (* z k)])

(defn v+ [& vs]
  (apply mapv + vs))

(defn v- [& vs]
  (apply mapv - vs))

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
    (if (or (not box-a) (not box-b))
      (throw (Exception. "No bounding box in bvh_node constructor.\n")))
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
     (if (or (not box-left)
             (not box-right))
       (throw (Exception. "No bounding box in bvh_node constructor.\n"))
       (->BvhNode left right (surrounding-box box-left box-right))))))

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

(defrecord Ray [orig dir time_])

(defprotocol Material
  (scatter [mat r-in rec]))

(defn ray
  ([orig dir] (ray orig dir 0.0))
  ([orig dir time_] (->Ray orig dir time_)))

(defn get-sphere-uv [this p u v]
  (let [theta (m/acos (- ^double (p 1)))
        phi (+ (m/atan2 (- ^double (p 2)) ^double (p 0)) m/PI)]
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
              (->HitRecord (ray-at r root) normal mat-ptr u v root front-face))
            (let [root (/ ^double (+ ^double (- ^double half-b) root) ^double a)]
              (when (> t-max root t-min)
                (let [outward-normal (vd (v- (ray-at r root) (center sphere time_)) radius)
                      {:keys [u v]} (get-sphere-uv sphere outward-normal (:u rec) (:v rec))
                      {:keys [front-face normal]} (set-face-normal r outward-normal)]
                  (->HitRecord (ray-at r root) normal mat-ptr u v root front-face)))))))))
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
              (->HitRecord (ray-at r root) normal mat-ptr u v root front-face))
            (let [root (/ ^double (+ ^double (- ^double half-b) root) ^double a)]
              (when (> t-max root t-min)
                (let [outward-normal (vd (v- (ray-at r root) center) radius)
                      {:keys [u v]} (get-sphere-uv sphere outward-normal (:u rec) (:v rec))
                      {:keys [front-face normal]} (set-face-normal r outward-normal)]
                  (->HitRecord (ray-at r root) normal mat-ptr u v root front-face)))))))))
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
       :attenuation (value albedo u v p)})))

(defn lambertian
  ([a] (->Lambertian (if (satisfies? Texture a) a
                       (solid-color a)))))

(defrecord Metal [albedo fuzz]
  Material
  (scatter [{:keys [albedo fuzz] :as mat}
            {:keys [orig dir time_] :as r-in}
            {:keys [normal p] :as rec}]
    (let [reflected (reflect (unit-vector dir) normal)
          scattered (ray p (v+ reflected (v* (random-in-unit-sphere) fuzz)) time_)
          ok (pos? ^double (dot (:dir scattered) normal))]
      {:ok ok :attenuation albedo :scattered scattered})))

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
      {:ok true :attenuation attenuation :scattered (ray p direction time_)})))

(defn ray-color [{:keys [_ dir] :as r} world ^double depth]
  (if (>= 0 depth)
    [0 0 0]
    (if-let [{:keys [mat-ptr] :as rec}
             (hit (->HittableList world) r 0.001 ##Inf {})]
          (let [{:keys [ok attenuation scattered]} (scatter mat-ptr r rec)]
            (if ok
              (map * attenuation
                   (ray-color scattered world (dec depth)))
              [0 0 0]))
      (let [[x y z] (unit-vector dir)
            t (* 0.5 (inc ^double y))]
        (v+ (v* [1.0 1.0 1.0] (- 1.0 t))
            (v* [0.5 0.7 1.0] t))))))

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
  (let [[uu vv ww] (mapv #(* ^double % ^double %
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
    (let [[u v w] (mapv #(let [value ^double (- % ^double (m/floor %))]
                           (* value value (- 3 ^double (* 2.0 ^double value))))
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

(defrecord NoiseTexture [perlin-noise scale]
  Texture
  (value [this u v p]
    (v* [1 1 1] (* 0.5 (inc (m/sin (+ (* scale (p 2))
                                      (* 10 (turb perlin-noise p 7)))))))))

(defn noise-texture
  ([] (->NoiseTexture (perlin) 1.0))
  ([sc] (->NoiseTexture (perlin) sc)))

(defn two-spheres []
  (let [checker (checker-texture [0.2 0.3 0.1]
                                 [0.9 0.9 0.9])]
    (z-fix [(->Sphere [0 -10 0] 10 (lambertian checker))
            (->Sphere [0 10 0] 10 (lambertian checker))])))

(defn two-perlin-spheres []
  (let [pertext (noise-texture 4.0)]
    (z-fix [(->Sphere [0 -1000 0] 1000 (lambertian pertext))
            (->Sphere [0 2 0] 2 (lambertian pertext))])))

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

;   (let [center2 (v+ center [0 (random-double 0 0.5) 0])]
;
;      (moving-sphere center center2 0.0 1.0 0.2 (lambertian (mapv * (random-vec3) (random-vec3)))))

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

(defrecord Worker [work latch]
  Runnable
  (run [this]
    (work)
    (.countDown latch)))

(defn -main [& {:keys [samples new-camera]
                :or   {samples 30 new-camera false}}]
  (let [aspect-ratio (/ 16.0 9.0)
        image-width 400
        image-height (/ image-width aspect-ratio)
        image-width-dec (dec image-width)
        image-height-dec (dec image-height)
        samples-per-pixel samples
        max-depth 50
        im :two-perlin-spheres                              ;world
        {:keys [world lookfrom lookat vfov aperture]}
        (im {:world              {:world    (random-scene)
                                  :lookfrom [13 2 3]
                                  :lookat   [0 0 0]
                                  :vfov     20.0
                                  :aperture 0.1}
             :two-spheres        {:world    (two-spheres)
                                  :lookfrom [13 2 3]
                                  :lookat   [0 0 0]
                                  :vfov     20.0
                                  :aperture 0.0}
             :two-perlin-spheres {:world    (two-perlin-spheres)
                                  :lookfrom [13 2 3]
                                  :lookat   [0 0 0]
                                  :vfov     20.0
                                  :aperture 0.0}})
        vup [0 1 0]
        dist-to-focus 10.0
        cam (camera lookfrom lookat vup vfov aspect-ratio aperture dist-to-focus 0.0 1.0)
        buffered-image (BufferedImage. 500 500 BufferedImage/TYPE_INT_RGB)
        latch (CountDownLatch. 225)]
    (dotimes [y image-height]
      (.start
        (Thread. ^Runnable
                 (->Worker
                   (fn []
                     (dotimes [x image-width]
                       (let [pixel-color
                             (reduce v+ [0 0 0]
                                     (repeatedly
                                       samples-per-pixel
                                       #(ray-color
                                          (get-ray cam
                                                   (/ ^double (+ ^long x ^double (rand))
                                                      image-width-dec)
                                                   (/ ^double (+ ^long y ^double (rand))
                                                      image-height-dec))
                                          world
                                          max-depth)))]
                         (let [scale (/ 1.0 ^long samples-per-pixel)
                               [r g b] (map (fn x-col [x] (* ^double (clamp ^double (m/sqrt x) 0.0 0.999) 255))
                                            (v* pixel-color scale))]
                           (.setRGB buffered-image x (- 256 y)
                                    (.getRGB (Color. ^int r
                                                     ^int g
                                                     ^int b)))))))
                   ;   (.countDown latch)))))
                   latch))))
    (.await latch)
    (display-image buffered-image image-width image-height)))

;"Elapsed time: 47368.8253 msecs" - 3x map
;"Elapsed time: 43142.843 msecs" - sequence
; memoize, 1apply -> reduce - pomale
; "Elapsed time: 43965.2278 msecs"
;"Elapsed time: 44464.4075 msecs"
;"Elapsed time: 75909.1767 msecs"
;"Elapsed time: 57100.1403 msecs"

; "Elapsed time: 36868.9088 msecs"
; "Elapsed time: 38390.163 msecs"
; "Elapsed time: 20547.4781 msecs"
;"Elapsed time: 25769.1286 msecs"
; "Elapsed time: 12041.8467 msecs"
;"Elapsed time: 10176.6658 msecs"
;"Elapsed time: 10864.6312 msecs" 10 samplu
; 20 samplu "Elapsed time: 29076.2112 msecs"

;kulicky (range -5 5), 1 samplu: "Elapsed time: 121312.5909 msecs"
;kulicky (range -2 2), 1 samplu: "Elapsed time: 14670.2477 msecs"
;"Elapsed time: 3082.055101 msecs"
;"Elapsed time: 4747.3713 msecs" with map
;"Elapsed time: 3408.6031 msecs" with sequence
;kulicky (range -2 2), 3 samplu: "Elapsed time: 10650.0589 msecs"
;"Elapsed time: 11340.6887 msecs"
;kulicky (range -2 2), 5 samplu:"Elapsed time: 16590.0575 msecs"
;"Elapsed time: 18843.5133 msecs"
;kulicky (range -2 2), 10 samplu:""Elapsed time: 30687.3569 msecs"
;;kulicky (range -2 2), 10 samplu: "Elapsed time: 106092.410901 msecs"

;;kulicky (range -3 3), 1 samplu: "Elapsed time: 9888.6378 msecs"
;2 samply: "Elapsed time: 10926.0386 msecs"
;;kulicky (range -3 3), 3 samplu:"Elapsed time: 31115.1872 msecs"
;;kulicky (range -3 3), 5 samplu: "Elapsed time: 76506.1908 msecs"
;"Elapsed time: 63893.7775 msecs"
;;kulicky (range -3 3), 10 samplu: "Elapsed time: 139513.187 msecs"

;;kulicky (range -4 4), 1 samplu: "Elapsed time: 27647.5078 msecs"
;;kulicky (range -5 5), 3 samplu: "Elapsed time: 127383.7256 msecs"
;;kulicky (range -11 11), 1 samplu: "Elapsed time: 88726.1296 msecs"
;;"Elapsed time: 159043.8817 msecs"
;"Elapsed time: 171207.5566 msecs"

;;kulicky (range -11 11), 1 samplu, hloubka 15: "Elapsed time: 175823.8208 msecs"
;sachovnice, staticke, hloubka 50: "Elapsed time: 138138.2704 msecs"

;;kulicky (range -11 11), 2 samplu: "Elapsed time: 345160.5857 msecs"
;sachovnice, staticke kulicky, hloubka 50: "Elapsed time: 253279.7235 msecs"

;;kulicky (range -11 11), 3 samplu, sachovnice: "Elapsed time: 683374.5917 msecs"

;4 base koule, 1 sampl, sequence
;"Elapsed time: 1057.2589 msecs"

;4 base koule, 1 sampl, map
;"Elapsed time: 1093.5785 msecs"
;"Elapsed time: 1054.9913 msecs"
;"Elapsed time: 1013.007 msecs"
;"Elapsed time: 1027.5259 msecs"

;5 base kouli, 15 samplu: "Elapsed time: 6864.0281 msecs"
;5 base kouli, 30 samplu: "Elapsed time: 45049.2963 msecs" 3 kovove: "Elapsed time: 55084.4118 msecs"

;(-2 2) (-2 2) => 30 kouli, kovove koule s vetsim radiusem, 5samplu: "Elapsed time: 25200.864 msecs"
;(-2 2) (-2 2) => 30 kouli, kovove koule s vetsim radiusem, 30samplu: "Elapsed time: 175474.8084 msecs"

;[r g b] (->> (v* pixel-color scale)
;             (map m/sqrt)
;             (map #(clamp % 0.0 0.999))
;             (map #(* ^double % 255)))

;sachovnice, 3 koule, depth 50
;"Elapsed time: 43229.0236 msecs"
;"Elapsed time: 42388.0452 msecs"
;"Elapsed time: 44769.0246 msecs"

;"Elapsed time: 45960.1954 msecs"
;"Elapsed time: 59640.5745 msecs"
;"Elapsed time: 53389.345 msecs"

;30 samplu 1 marble, 50 depth
;(time (-main))
;"Elapsed time: 286693.2659 msecs"
;=>
;#object[javax.swing.JFrame
;        0x782a422a
;        "javax.swing.JFrame[frame5,0,0,420x300,layout=java.awt.BorderLayout,title=Ray Tracing,resizable,normal,defaultCloseOperation=HIDE_ON_CLOSE,rootPane=javax.swing.JRootPane[,8,31,404x261,layout=javax.swing.JRootPane$RootLayout,alignmentX=0.0,alignmentY=0.0,border=,flags=16777673,maximumSize=,minimumSize=,preferredSize=],rootPaneCheckingEnabled=true]"]
