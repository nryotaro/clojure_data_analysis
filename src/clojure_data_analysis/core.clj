(ns clojure-data-analysis.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [criterium.core :as q]
            [clojure.core.reducers :as r]))
;  (:require [clojure.string :as str :refer [replace]]

(defn count-words
  ([] {})
  ([freqs word]
    (assoc freqs word (inc (get freqs word 0)))))

(defn merge-counts
  ([] {})
  ([& m] (apply merge-with + m)))


; make upper-case
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
(defn gen-az [] (lazy-seq (cons (str (char (+ (int \a) (rand-int 26)))) (gen-az))))

(defn gen-az2 [] (lazy-seq (cons (do (println "!")(str (char (+ (int \a) (rand-int 26))))) (gen-az2))))

(defn wc
  [lst]
  (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} lst))

(defn wc-p [lst chunk-size]
  (let [parts (partition-all (int (/ (count lst) chunk-size)) lst)]
  (apply merge-with + (pmap wc parts))))


(defn fb
  [n]
  (->> (range 1 n)
       (map fizzbuzz)
       (filter string?)
       (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {})
       ))

(defn fb2
  [n c]
  (->> (range 1 n)
       (r/map fizzbuzz)
       (r/filter string?)
       (r/fold c merge-counts count-words)))  

(defn rand-point [] [(rand) (rand)])

(defn center-dist [[x y]]
  (Math/sqrt (+ (* x x) (* y y))))

(defn count-in-circle
  [n]
  (->> (repeatedly n rand-point) (map center-dist) (filter #(<= % 1.0)) count))

(defn count-in-circle2
  [n]
  (->> (repeatedly n rand-point) (r/map center-dist) (r/filter #(<= % 1.0)) count))

(defn calc-pi
  [n]
  (letfn [(dist-from-origin [[x y]] (Math/sqrt (+ (* x x) (* y y))))
          (count-in-circle [dots] (count (filter #(<= % 1.0) dots)))]
    (let [dots (map dist-from-origin (repeatedly n (fn [] [(rand) (rand)])))]
      (* 4.0 (/ (count-in-circle dots) n)))))

(defn calc-pi2
  [n chunk-size]
  (letfn [(dist-from-origin [[x y]] (Math/sqrt (+ (* x x) (* y y))))
          (count-in-circle [dots] (count (filter #(<= % 1.0) dots)))]
    (let [chunks (partition-all chunk-size (repeatedly n (fn [] [(rand) (rand)])))
          dots-in-circle (reduce + 0 (pmap #(count-in-circle (map (fn [e] (dist-from-origin e)) %)) chunks))]
      (* 4.0 (/ dots-in-circle n)))))



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
   (+ acc e)))
(defn my-pi
  [n]
  (let [c (->> (repeatedly n rand-point)
               (r/filter #(<= (center-dist %) 1.0))
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

(defn get-temp [r] (- 1.0 (float r)))

(defn get-neighbor
  [state]
  (max 1 (min 20 (+ state (- (rand-int 11) 5)))))

(defn get-wc-cost
  [n state]
  (let [chunk-size (long (Math/pow 2 state))]
    (first (:mean (q/quick-benchmark
                   (wc-p (take n (gen-az)) (min chunk-size n))
                   {})))))

(defn get-wc-cost2
  [lst state]
  (-> (q/quick-benchmark (wc-p lst state) {}) :mean first))

(def words (take 1000000 (gen-az)))

(def get-wc-cst
  (memoize (fn [state] (-> (q/quick-benchmark (wc-p words state) {}) :mean first))))


(defn should-move
  [c0 c1 t]
  (* t (if (< c0 c1) 0.25 1.0)))

(defn annealing
  [initial max-iter max-cost
   neighbor-fn cost-fn p-fn temp-fn]
  (let [get-cost (memoize cost-fn)
        cost (get-cost initial)]
    (loop [state initial
           cost cost
           k 1
           best-seq [{:state state, :cost cost}]]
      (println '>>> 'sa k \. state \$ cost)
      (if (and (< k max-iter)
               (or (nil? max-cost)
                   (> cost max-cost)))
        (let [t (temp-fn (/ k max-iter))
              next-state (neighbor-fn state)
              next-cost (get-cost next-state)
              next-place {:state next-state,
                          :cost next-cost}]
          (if (> (p-fn cost next-cost t) (rand))
            (recur next-state next-cost (inc k)
                   (conj best-seq next-place))
            (recur state cost (inc k) best-seq)))
        best-seq))))

(defn annealing2
  [init max-iter neighbor-fn cost-fn p-fn temp-fn]
  (let [init-cost (cost-fn init)]
    (loop [state init
           cost init-cost
           k 1
           best-seq [{:state state, :cost cost}]]
      (println '>>> 'sa k \. state \$ cost)
      (if (<= k max-iter)        
        (let [t (temp-fn (/ k max-iter))
              next-state (neighbor-fn state)
              next-cost (cost-fn next-state)
              next-place {:state next-state,
                          :cost next-cost}]
          (if (> (p-fn cost next-cost t) (rand))
            (recur next-state next-cost (inc k)
                   (conj best-seq next-place))
            (recur state cost (inc k) best-seq)))
        best-seq))))

;(annealing 10 10 nil get-neighbor (partial get-wc-cost 1000000) should-move get-temp)
