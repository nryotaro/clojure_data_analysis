(ns clojure-data-analysis.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [criterium.core :as q]
            [clojure.core.reducers :as r]))
;  (:require [clojure.string :as str :refer [replace]]

(defn count-words
  ([] {})
  ([freqs word]
    (println "count word")
    (assoc freqs word (inc (get freqs word 0)))))

(defn merge-counts
  ([] {})
  ([& m] (apply merge-with + m)))

(defn word-frequency [text]
  (r/fold merge-counts count-words (clojure.string/split text #"\s+")))

(defn texts [filename] (str/split (slurp (io/resource filename)) #"\r?\n"))

(defn chunks
  [texts]
  (map #(str/split % #"\s+") texts))

(defn fizzbuzz
  [n]
  (cond
    (= (mod n 15) 0) "FizzBuzz"
    (= (mod n 3) 0) "Fizz"
    (= (mod n 5) 0) "Buzz"
    :else n))
                                        ; (q/quick-bench (map #(map (fn [e] (fizzbuzz e)) %) (partition-all 500000 (range 1 1000000))))

  

(defn rand-point [] [(rand) (rand)])

(defn center-dist [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn count-in-circle
  [n]
  (->> (repeatedly n rand-point) (map center-dist) (filter #(<= % 1.0)) count))

(defn count-in-circle2
  [n]
  (->> (repeatedly n rand-point) (r/map center-dist) (r/filter #(<= % 1.0)) count))

(defn mc-pi
  [n]
  (* 4.0 (/ (count-in-circle n) n)))

(defn mc-pi2
  [n]
  (* 4.0 (/ (count-in-circle2 n) n)))

(defn in-circle-flag
  [p]
  (if (<= (center-dist p) 1.0) 1 0))

(defn mc-pi-pmap
  [n]
  (let [in-circle (->> (repeatedly n rand-point)
                       (pmap in-circle-flag)
                       (reduce + 0))]
    (* 4.0 (/ in-circle n))))

(defn mc-pi-pmap2
  [n]
  (let [in-circle (->> (repeatedly n rand-point)
                       (r/map in-circle-flag)
                       (reduce + 0))]
    (* 4.0 (/ in-circle n))))

(defn temp
  ([]
   0)
  ([acc e]
   (println "temp")
   (+ acc e)))
(defn my-pi
  [n]
  (let [c (->> (repeatedly n rand-point)
               (r/map #(do (println "ccc") (if (<= (center-dist %) 1.0) 1 0)))
               (r/fold 10 temp temp))]
    (* 4.0 (/ c n))))

; use reducers

(defn mc-pi-part
  ([n] (mc-pi-part 512 n))
  ([chunk-size n]
   (let [step (int
               (Math/floor (float (/ n chunk-size))))
         remainder (mod n chunk-size)
         parts (lazy-seq
                (cons remainder
                      (repeat step chunk-size))) ; (chunk-size chunk-size ....)
         in-circle (reduce + 0
                           (pmap count-in-circle
                                 parts))]
     (* 4.0 (/ in-circle n)))))

(def chunk-size 4096)

(def input-size 1000000)

(def fib-seq (lazy-cat [0 1] (map + (rest fib-seq) fib-seq)))
