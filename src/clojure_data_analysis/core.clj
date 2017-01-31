(ns clojure-data-analysis.core)

(defn get-escape-point [scaled-x scaled-y max-iterations]
  (loop [x 0
         y 0
         iteration 0]
    (let [x2 (* x x) 
          y2 (* y y)]
      (if (and (< (+ x2 y2) 4) (< iteration max-iterations))
        (recur (+ (- x2 y2) scaled-x) (+ (* 2 x y) scaled-y) (inc iteration))
        iteration))))

(defn scale-to [pixel maximum [lower upper]]
  (+ (* (/ pixel maximum)
        (Math/abs (- upper lower))) 
     lower))

(defn scale-point [pixel-x pixel-y max-x max-y set-range]
  [(scale-to pixel-x max-x (:x set-range))
   (scale-to pixel-y max-y (:y set-range))])

(defn output-points [max-x max-y]
  ; (c/output-points 2 2) => ([0 0] [0 1] [1 0] [1 1])
  (let [range-y (range max-y)]
  	; (mapcat reverse [[3 2 1 0] [6 5 4] [9 8 7]]) => (0 1 2 3 4 5 6 7 8 9)
    (mapcat (fn [x] (map #(vector x %) range-y)) (range max-x))))

(defn mandelbrot-pixel
  ([max-x max-y max-iterations set-range]
   (partial mandelbrot-pixel max-x max-y max-iterations set-range))
  ([max-x max-y max-iterations set-range [pixel-x pixel-y]]
   (let [[x y] (scale-point pixel-x pixel-y max-x max-y set-range)]
     (get-escape-point x y max-iterations))))

(defn mandelbrot [mapper max-iterations max-x max-y set-range]
  ;; doall forces the seq to be realized
  (doall (mapper 
           (mandelbrot-pixel max-x max-y max-iterations set-range) 
           (output-points max-x max-y))))

(def mandelbrot-range {:x [-2.5, 1.0], :y [-1.0, 1.0]})
;  (q/quick-bench (c/mandelbrot map 500 1000 1000 c/mandelbrot-range))