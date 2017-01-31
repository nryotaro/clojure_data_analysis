(ns clojure-data-analysis.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.core.reducers :as r]))
;  (:require [clojure.string :as str :refer [replace]]

(defn count-words
  ([] {})
  ([freqs word]
    (assoc freqs word (inc (get freqs word 0)))))

(defn merge-counts
  ([] {})
  ([& m] (apply merge-with + m)))

(defn word-frequency [text]
  (r/fold merge-counts count-words (clojure.string/split text #"\s+")))

(defn texts [] (str/split (slurp (io/resource "text.txt")) #"\r?\n"))
