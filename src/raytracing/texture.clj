(ns raytracing.texture
  (:require
    [raytracing
     [base :refer :all]
     [noise :refer :all]]
    [clojure.math :as m])
  (:import (java.awt.image DataBufferByte)
           (java.net URL)
           (javax.imageio ImageIO))
  (:gen-class))

(defprotocol Texture
  (value [this u v p]))

(defrecord SolidColor [color-value]
  Texture
  (value [this u v p] color-value))

(defn solid-color
  ([c] (->SolidColor c))
  ([red green blue] (->SolidColor [red green blue])))

(defrecord CheckerTexture [odd even]
  Texture
  (value [this u v p]
    (let [sines (transduce (map #(m/sin (* 10 ^double %)))
                           * p)]
      (if (neg? ^double sines)
        (value odd u v p)
        (value even u v p)))))

(defn checker-texture [c1 c2]
  (if (satisfies? Texture c1)
    (->CheckerTexture c1 c2)
    (->CheckerTexture (solid-color c1)
                      (solid-color c2))))

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

(defrecord ImageTexture [data
                         ^long width
                         ^long height
                         bytes-per-scanline]
  Texture
  (value [this u v p]
    (if-not data
      [0 1 1]
      (let [uu ^double (clamp u 0.0 1.0)
            vv (- 1.0 ^double (clamp v 0.0 1.0))
            i (int (* uu width))
            j (int (* vv height))
            i (if (>= i width) (dec width) i)
            j (if (>= j height) (dec height) j)
            color-scale (/ 1.0 255.0)]
        (v* (get-in data [j i] [0.0 0.0 0.0])
            color-scale)))))

(defn image-texture
  ([] (->ImageTexture nil 0 0 0))
  ([^URL filename-url]
   (let [bytes-per-pixel 3
         image (ImageIO/read filename-url)
         buffer (.getDataBuffer (.getRaster image))
         data (->> (.getData ^DataBufferByte buffer)
                   (map (fn [^long n] (if (neg? n) (+ 256 n) n)))
                   (partitionv 3)
                   (mapv rseq))
         w (.getWidth image)
         h (.getHeight image)]
     (->ImageTexture (vec (partitionv w data))
                     w h (* bytes-per-pixel w)))))