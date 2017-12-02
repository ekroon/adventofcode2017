(ns adventofcode.day2)

(defn input []
  (clojure.java.io/reader (clojure.java.io/resource "day2/input")))

(defn example-part1 []
  (clojure.java.io/reader (clojure.java.io/resource "day2/example-part1")))

(defn example-part2 []
  (clojure.java.io/reader (clojure.java.io/resource "day2/example-part2")))

(defn line->numbers [str]
  (map #(Integer/parseInt %) (re-seq #"\d+" str)))

(defn line-value-1 [line-seq]
  (- (apply max line-seq)
     (apply min line-seq)))

(defn line-value-2 [line-seq]
  (apply max
         (for [x     line-seq
               y     line-seq
               :let  [min_ (min x y)
                      max_ (max x y)]
               :when (= 0 (mod max_ min_))]
           (/ max_ min_))))

(defn solve [reader' line-value-fn]
  (as-> (line-seq reader') $
    (map (comp line-value-fn line->numbers) $)
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
