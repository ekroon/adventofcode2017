(ns adventofcode.day4)

(defn input []
  (clojure.java.io/reader (clojure.java.io/resource "day4/input")))

(defn example-part1 []
  (clojure.java.io/reader (clojure.java.io/resource "day4/example-part1")))

(defn example-part2 []
  (clojure.java.io/reader (clojure.java.io/resource "day4/example-part2")))

(defn line->words [str]
  (re-seq #"\w+" str))

(defn line-value-1 [words]
  (if (every? #{1} (vals (frequencies words)))
    1
    0))

(defn line-value-2 [words]
  (if (every? #{1} (vals (frequencies (map #(str (sort %)) words))))
    1
    0))

(defn solve [reader' line-value-fn]
  (as-> (line-seq reader') $
    (map (comp line-value-fn line->words) $)
    (reduce + $)))

(defn solve-1 [reader']
  (solve reader' line-value-1))

(defn solve-2 [reader']
  (solve reader' line-value-2))

;; (solve-1 (input))
;; (solve-2 (input))

(defn -main [& args]
  (println "part1: " (solve-1 (input)))
  (println "part2: " (solve-2 (input))))
