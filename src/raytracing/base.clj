(ns raytracing.base
  (:require [clojure.java.io :as io]
            [clojure.math :as m])
  (:gen-class))

(set! *unchecked-math* :warn-on-boxed)
(set! *warn-on-reflection* true)

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
                    t0 ^double (* invD (- ^double (minimum a) ^double (orig a)))
                    t1 ^double (* invD (- ^double (maximum a) ^double (orig a)))
                    temp t1
                    t1 (if (< invD 0.00) t0 t1)
                    t0 (if (< invD 0.00) temp t0)
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

(defn hittable-list [objects]
  (->HittableList objects))

(defrecord BvhNode [left right box]
  Hittable
  (hit [this r t-min t-max rec]
    (when (hit box r t-min t-max rec)
      (let [{:keys [t] :as hit-left} (hit left r t-min t-max rec)
            hit-right (hit right r t-min (if hit-left t t-max) rec)]
        (or hit-right hit-left))))
  (bounding-box [this time0 time1 output-box-ptr]
    box))

(defn random-double
  ([] (Math/random))
  ([^double mn ^double mx] (+ mn (* (- mx mn) ^double (Math/random)))))

(defn random-int [min_ max_]
  (int (random-double min_ (inc ^double max_))))

(defn box-compare [axis]
  (fn [a b] (< ^double ((:minimum (bounding-box a 0 0 nil)) axis)
               ^double ((:minimum (bounding-box b 0 0 nil)) axis))))

(defn bvhnode
  ([list_ time0 time1] (bvhnode (:objects list_)
                                0 (count (:objects list_))
                                time0 time1))
  ([src-objects start end time0 time1]
   (let [objects (vec src-objects)
         axis (rand-int 2)
         comparator_ (box-compare axis)
         object-span ^long (- ^long end
                              ^long start)
         [left right] (cond (= object-span 1)
                            [(objects start) (objects start)]
                            (= object-span 2)
                            (if (comparator_ (objects start)
                                             (objects (inc ^long start)))
                              [(objects start)
                               (objects (inc ^long start))]
                              [(objects (inc ^long start))
                               (objects start)])
                            :else (let [sorted-obj (sort-by #((:minimum (bounding-box % 0 0 nil)) axis)
                                                            < objects)
                                        mid (int (Math/ceil (+ ^long start
                                                               ^double (/ object-span 2))))]
                                    [(bvhnode sorted-obj start mid time0 time1)
                                     (bvhnode sorted-obj mid end time0 time1)]))
         box-left (bounding-box left time0 time1 nil)
         box-right (bounding-box right time0 time1 nil)]
       (->BvhNode left right (surrounding-box box-left box-right)))))

(defn ray-at [{:keys [orig dir]} t]
  (v+ orig (v* dir t)))

(defrecord HitRecord [p normal mat-ptr t u v front-face])

(defn set-face-normal [{:keys [orig dir] :as r} outward-normal]
  (let [front-face (neg? ^double (dot dir outward-normal))]
    {:front-face front-face
     :normal     (if front-face outward-normal (map - outward-normal))}))

(defn random-vec3
  ([] [(Math/random) (Math/random) (Math/random)])
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

;(defn near-zero [v3]
;  (let [s 1e-8]
;    (every? #(> s ^double (abs ^double %)) v3)))

(defn reflectance [^double cosine ^double ref-idx]
  (let [r0 (m/pow
             (/ (- 1.0 ref-idx)
                (+ 1.0 ref-idx)) 2)]
    (+ r0 (* (- 1.0 r0)
             (m/pow (- 1 cosine) 5)))))

(defn get-sphere-uv [this [x y z] u v]
  (let [theta (m/acos (- ^double y))
        phi (+ (m/atan2 (- ^double z) ^double x)
               m/PI)]
    {:u (/ phi (* 2 m/PI))
     :v (/ theta m/PI)}))

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
  (bounding-box [{:keys [center radius]} time0 time1 output-box]
    (aabb (v- center
              [radius radius radius])
          (v+ center
              [radius radius radius])))
  (center [this time_] center))

(defn sphere [center radius mat-ptr]
  (->Sphere center radius mat-ptr))

(defrecord MovingSphere [center0 center1
                         ^double time0
                         ^double time1
                         ^double radius
                         mat-ptr]
  Hittable
  (hit [{:keys [radius mat-ptr] :as this}
        {:keys [orig dir time_] :as r}
        t-min
        t-max
        rec]
    (let [oc (v- orig (center this time_))
          a (length-squared dir)
          half-b ^double (dot oc dir)
          c (- ^double (length-squared oc) ^double (m/pow radius 2))
          discriminant (- ^double (m/pow half-b 2) ^double (* ^double a ^double c))]
      (when (pos? ^double discriminant)
        (let [sqrtd (m/sqrt discriminant)
              root (/ ^double (- (- ^double half-b) ^double sqrtd) ^double a)]
          (if (> t-max root t-min)
            (let [outward-normal (vd (v- (ray-at r root) (center this time_)) radius)
                  {:keys [u v]} (get-sphere-uv this outward-normal (:u rec) (:v rec))
                  {:keys [front-face normal]} (set-face-normal r outward-normal)]
              (->HitRecord (ray-at r root) normal mat-ptr root u v front-face))
            (let [root (/ ^double (+ ^double (- ^double half-b) root) ^double a)]
              (when (> t-max root t-min)
                (let [outward-normal (vd (v- (ray-at r root) (center this time_)) radius)
                      {:keys [u v]} (get-sphere-uv this outward-normal (:u rec) (:v rec))
                      {:keys [front-face normal]} (set-face-normal r outward-normal)]
                  (->HitRecord (ray-at r root) normal mat-ptr root u v front-face)))))))))
  (bounding-box [this time0_ time1_ output-box]
    (surrounding-box (aabb (v- (center this time0_) [radius radius radius])
                           (v+ (center this time0_) [radius radius radius]))
                     (aabb (v- (center this time1_) [radius radius radius])
                           (v+ (center this time1_) [radius radius radius]))))
  (center [this time_]
    (v+ center0
        (v* (v- center1 center0)
            (/ (- ^double time_ ^double time0)
               (- ^double time1 ^double time0))))))

(defn moving-sphere [center0 center1 time0 time1 radius mat_ptr]
  (->MovingSphere center0 center1 time0 time1 radius mat_ptr))



(defn clamp [^double x ^double mn ^double mx]
  (cond (> mn x) mn
        (> x mx) mx
        :else x))