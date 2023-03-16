(ns raytracing.core
  (:require
    [clojure.math :as m]
    [clojure.math.combinatorics :as c]
    [clojure.java.io :as io]
    [raytracing.base :refer :all]
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

(defn -main [& {:keys [samples new-camera]
                :or   {samples 10 new-camera false}}]
  (let [aspect-ratio 1.0;(/ 16.0 9.0) ;1.0
        image-width 200 ;400, 600
        image-height (long (/ image-width aspect-ratio))
        image-width-dec (dec ^long image-width)
        image-height-dec (dec ^long image-height)
        samples-per-pixel samples
        max-depth 50
        im :final-scene                              ;:two-perlin-spheres                              ;world
        {:keys [world lookfrom lookat vfov aperture background]
         :or   {lookfrom [13 2 3]
                lookat   [0 0 0]
                aperture 0.0
                vfov     20.0
                background [0.70 0.80 1.0]}}
        (im {:world              {:world    (random-scene)
                                  :aperture 0.1}
             :two-spheres        {:world (two-spheres)}
             :two-perlin-spheres {:world (two-perlin-spheres)}
             :earth              {:world (earth)}
             :simple-light {:world (simple-light)
                            :background [0.0 0.0 0.0]
                            :lookfrom [26 3 6]
                            :lookat [0 2 0]}
             :cornell-box {:world (cornell-box)
                           :background [0.0 0.0 0.0]
                           :lookfrom [278 278 -800]
                           :lookat [278 278 0]
                           :vfov 40.0}
             :cornell-smoke {:world (cornell-smoke)
                             :lookfrom [278 278 -800]
                             :lookat [278 278 0]
                             :vfov 40.0}
             :final-scene {:world (final-scene)
                           :background [0 0 0]
                           :lookfrom [478 278 -600]
                           :lookat [278 278 0]
                           :vfov 40}})
        vup [0 1 0]
        dist-to-focus 10.0
        cam (camera lookfrom lookat vup vfov aspect-ratio aperture dist-to-focus 0.0 1.0) ;500 500
        buffered-image (BufferedImage. 1000
                                       1000
                                       BufferedImage/TYPE_INT_RGB)
        latch (CountDownLatch. image-height)]
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
                                          background
                                          world
                                          max-depth)))]
                         (let [scale (/ 1.0 ^long samples-per-pixel)
                               [r g b] (map (fn [x] (* ^double (clamp ^double (m/sqrt x) 0.0 0.999) 255))
                                            (v* pixel-color scale))]
                           (.setRGB buffered-image x (- ^long (+ ^long image-height 30) y)
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

;two-spheres
;(time (-main))
;"Elapsed time: 23116.2879 msecs"

;(time (-main))
;"Elapsed time: 13424.315 msecs"

;(time (-main))
;"Elapsed time: 9307.8736 msecs"

;(time (-main))
;"Elapsed time: 7960.1227 msecs"

;(time (-main)) 10 samplu
;"Elapsed time: 6405.354 msecs"

;(time (-main)) ;10 sp, nekde vektory
;"Elapsed time: 12776.2304 msecs"

;2 kostky, 100 samplu na 400
;(time (-main))
;"Elapsed time: 415119.3728 msecs"
