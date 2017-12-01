(ns adventofcode.day1)

(def input
  (slurp (clojure.java.io/resource "day1/input")))

(defn number-compare [[first' second']]
  (let [parsed-first  (Character/getNumericValue first')
        parsed-second (Character/getNumericValue second')]
    (if (= parsed-first parsed-second) parsed-first 0)))

(defn solve [input add-to-index-fn]
  (as-> (into [] input) $
    (map-indexed
     (let [length (count $)
           add-to-index (add-to-index-fn length)]
       (fn [i v]
         [v (nth $ (mod (+ i add-to-index) length))]))
     $)
    (map number-compare $)
    (reduce + $)))

(defn solve-1 [input]
  (solve input (fn [_] 1)))

(defn solve-2 [input]
  (solve input (fn [length] (/ length 2))))

 ;; (solve-1 input)
 ;; (solve-2 input)

(defn -main [& args]
  (println "part1: " (solve-1 input))
  (println "part2: " (solve-2 input)))
