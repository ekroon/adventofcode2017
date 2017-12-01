(ns adventofcode.day1)

(def input
  (slurp (clojure.java.io/resource "day1/input")))

(defn number-compare [[first' second']]
  (let [parsed-first  (Character/getNumericValue first')
        parsed-second (Character/getNumericValue second')]
    (if (= parsed-first parsed-second) parsed-first 0)))

(defn solve-1 [input]
  (as-> (concat input [(first input)]) $
        (partition 2 1 $)
        (map number-compare $)
        (reduce + $)))

(defn solve-2 [input]
  (as-> (into [] input) $
        (map-indexed
         (fn [i v]
           (let [length (count $)
                 half-length (/ length 2)]
             [v (nth $ (mod (+ i half-length) length))]))
         $)
        (map number-compare $)
        (reduce + $)))

;; (solve-1 input)
;; (solve-2 input)

(defn -main [& args]
  (println "part1: " (solve-1 input))
  (println "part2: " (solve-2 input)))
