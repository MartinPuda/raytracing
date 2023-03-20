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
           (java.util.concurrent CountDownLatch)
           (raytracing utils)
           (clojure.lang IFn))
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
        im :final-scene                                    ;:two-perlin-spheres                              ;world
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

;two spheres- 25 samplu, 400 w, depth 50
;"Elapsed time: 58928.5308 msecs"
;(time (-main))
;"Elapsed time: 41847.0845 msecs"
;(time (-main))
;"Elapsed time: 37102.6371 msecs"
;(time (-main))
;"Elapsed time: 47579.758 msecs"

;two spheres s kusem v Jave
;"Elapsed time: 34865.9304 msecs"
;"Elapsed time: 37375.4996 msecs"
;"Elapsed time: 38493.4375 msecs"

;cornell-box s kusem v Jave, 25 samplu na 400
;"Elapsed time: 110203.4634 msecs"

;cornell-box bez Javy amap, 25 samplu na 400
;"Elapsed time: 94163.3877 msecs"
;"Elapsed time: 90874.5917 msecs"
;"Elapsed time: 88458.791801 msecs" (s Math/random)

;cornell-box s kusem v Jave, 50 samplu na 200
;"Elapsed time: 54061.5173 msecs"

;cornell-box bez Javy amap, 50 samplu na 200
;"Elapsed time: 42820.3253 msecs"
;"Elapsed time: 44497.7521 msecs"

;cornell-box s kusem v Jave, 100 samplu na 200
;"Elapsed time: 110612.1648 msecs"

;cornell-box s kusem v Jave, 200 samplu na 200
;"Elapsed time: 239491.3995 msecs"

;cornell-box bez Javy, 200 samplu na 200
;"Elapsed time: 202324.796 msecs"
;"Elapsed time: 183997.7448 msecs"

;cornell-box bez Javy amap, 200 samplu na 200
;"Elapsed time: 209734.0942 msecs"

;setpixels
;(time (-main))
;"Elapsed time: 35274.5043 msecs"
;(time (-main))
;"Elapsed time: 40456.5618 msecs"

;gulocky, 500 samplu na 400
;(time (-main))
;"Elapsed time: 665987.0818 msecs"

;final gulocky, 10sp na 400
;"Elapsed time: 14102.4992 msecs"
;"Elapsed time: 18790.5017 msecs"
;"Elapsed time: 31063.8751 msecs"

;final gulocky, 10samplu na 400 w/ loop
;"Elapsed time: 10761.8985 msecs"
;"Elapsed time: 13142.283601 msecs"
;"Elapsed time: 13489.7327 msecs"
;"Elapsed time: 16093.078399 msecs"
;"Elapsed time: 21211.102899 msecs"

;final gulocky, 10samplu na 400 w/ loop, vlastni rgb prevod
;"Elapsed time: 11745.8601 msecs"
;"Elapsed time: 12572.0935 msecs"
;"Elapsed time: 12609.4836 msecs"
;"Elapsed time: 13580.5856 msecs"
;"Elapsed time: 16112.0983 msecs"

;final gulocky, 10samplu na 400 w/ loop, loop akumulace a nasledny convert int-array
;"Elapsed time: 11873.1116 msecs"
;"Elapsed time: 12152.4105 msecs"
;"Elapsed time: 12451.2062 msecs"
;"Elapsed time: 12614.6914 msecs"
;"Elapsed time: 13864.8733 msecs"
;"Elapsed time: 15142.1228 msecs"
;"Elapsed time: 15879.3622 msecs"
;"Elapsed time: 17035.1659 msecs" II
;"Elapsed time: 17133.8049 msecs"
;"Elapsed time: 18154.5528 msecs" II
;"Elapsed time: 19624.4983 msecs"
;"Elapsed time: 21850.4542 msecs" II

;final gulocky, 10samplu na 400 w/ loop, loop akumulace a amap
;"Elapsed time: 12149.7333 msecs"
;"Elapsed time: 14120.275 msecs"
;"Elapsed time: 15675.0424 msecs"
;"Elapsed time: 16111.7951 msecs"

;final gulocky, 10 samplu na 400 w/loop a kusem v Jave
;"Elapsed time: 11041.6555 msecs"
;"Elapsed time: 11106.0236 msecs"
;"Elapsed time: 11147.6397 msecs"
;"Elapsed time: 11891.557 msecs"
;"Elapsed time: 11952.4203 msecs"
;"Elapsed time: 27538.8398 msecs"

;final scene, 5 samples, 400
;"Elapsed time: 8879.9814 msecs"
;"Elapsed time: 11506.7353 msecs"
;"Elapsed time: 12377.4107 msecs"

; final scene, 500 samples na 400 (rozhodne vic nez 25 minut)
;"Elapsed time: 2807163.361 msecs"