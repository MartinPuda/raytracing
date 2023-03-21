(ns raytracing.core
  (:require
    [clojure.math :as m]
    [clojure.math.combinatorics :as c]
    [raytracing.base :refer :all]
    [raytracing.camera :refer :all]
    [raytracing.rectangles :refer :all]
    [raytracing.scenes :refer :all]
    [raytracing.box :refer :all]
    [raytracing.ray :refer :all]
    [raytracing.materials :refer :all]
    [raytracing.translate :refer :all])
  (:import (javax.swing JFrame JPanel)
           (java.awt Dimension BorderLayout Color)
           (java.awt.image BufferedImage)
           (java.util.concurrent CountDownLatch))
  (:gen-class))

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

(defrecord Worker [work latch]
  Runnable
  (run [this]
    (work)
    (.countDown latch)))

(defn -main [& {:keys [samples]
                :or   {samples 5}}]
  (let [aspect-ratio 1.0                                    ;(/ 16.0 9.0) ;1.0
        image-width 200                                     ;400, 600
        image-height (long (/ image-width aspect-ratio))
        image-width-dec (dec ^long image-width)
        image-height-dec (dec ^long image-height)
        samples-per-pixel samples
        max-depth 50
        im  :final-scene
        {:keys [world lookfrom lookat vfov aperture background]
         :or   {lookfrom   [13 2 3]
                lookat     [0 0 0]
                aperture   0.0
                vfov       20.0
                background [0.70 0.80 1.0]}}
        (im {:world              {:world    (random-scene)
                                  :aperture 0.1}
             :two-spheres        {:world (two-spheres)}
             :two-perlin-spheres {:world (two-perlin-spheres)}
             :earth              {:world (earth)}
             :simple-light       {:world      (simple-light)
                                  :background [0.0 0.0 0.0]
                                  :lookfrom   [26 3 6]
                                  :lookat     [0 2 0]}
             :cornell-box        {:world      (cornell-box)
                                  :background [0.0 0.0 0.0]
                                  :lookfrom   [278 278 -800]
                                  :lookat     [278 278 0]
                                  :vfov       40.0}
             :cornell-smoke      {:world    (cornell-smoke)
                                  :lookfrom [278 278 -800]
                                  :lookat   [278 278 0]
                                  :vfov     40.0}
             :final-scene        {:world      (final-scene)
                                  :background [0.0 0.0 0.0]
                                  :lookfrom   [478 278 -600]
                                  :lookat     [278 278 0]
                                  :vfov       40}})
        vup [0 1 0]
        dist-to-focus 10.0
        cam (camera lookfrom lookat vup vfov aspect-ratio aperture dist-to-focus 0.0 1.0) ;500 500
        buffered-image (BufferedImage. 1000
                                       1000
                                       BufferedImage/TYPE_INT_RGB)
        latch (CountDownLatch. image-height)
        rc (fn [^long x ^long y]
             (ray-color
               (get-ray cam
                        (/ ^double (+ ^long x ^double (rand))
                           image-width-dec)
                        (/ ^double (+ ^long y ^double (rand))
                           image-height-dec))
               background
               world
               max-depth))
        scale (/ 1.0 ^long samples-per-pixel)
        clamp-fn (fn [^double x] (clamp ^double (m/sqrt x) 0.0 0.999))
        rgb-fn (fn [[^float r ^float g ^float b]] (.getRGB (Color. r g b)))]
    (dotimes [y image-height]
      (.start
        (Thread. ^Runnable
                 (->Worker
                   (fn []
                     (let [pxs (amap ^ints (int-array image-width) idx ret
                                     (->> (loop [s 0
                                                 acc [0.0 0.0 0.0]]
                                            (if (= s samples-per-pixel)
                                              (v* acc scale)
                                              (recur (inc s)
                                                     (v+ acc (rc idx y)))))
                                          (map clamp-fn)
                                          rgb-fn))]
                       (.setRGB buffered-image 0
                                (- ^long (+ ^long image-height 30) y)
                                image-width 1 ^ints pxs
                                0 0)))
                   latch))))
    (.await latch)
    (display-image buffered-image image-width image-height)))