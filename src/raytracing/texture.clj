(ns raytracing.texture
  (:require [raytracing.base :refer :all])
  (:import (java.awt.image DataBufferByte)
           (java.net URL)
           (javax.imageio ImageIO))
  (:gen-class))

(defrecord ImageTexture [data
                         ^long width
                         ^long height
                         bytes-per-scanline]
  Texture
  (value [this u v p]
    (if-not data
      [0 1 1]
      (let [;v (/ v 10.0) ;bytes-per-pixel 3
            uu ^double (clamp u 0.0 1.0)
            vv (- 1.0 ^double (clamp v 0.0 1.0))
            ; (clamp v 0.0 1.0)
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
                   (partition 3)
                   (mapv (comp vec reverse)))
         w (.getWidth image)
         h (.getHeight image)]
     (->ImageTexture (vec (partitionv w data))
                     w h (* bytes-per-pixel w)))))