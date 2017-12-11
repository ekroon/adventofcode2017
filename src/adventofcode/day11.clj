(ns adventofcode.day11)

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; [x y]
(def directions {'n  [ 0  2]
                 'ne [ 1  1]
                 'nw [-1  1]
                 's  [ 0 -2]
                 'se [ 1 -1]
                 'sw [-1 -1]})

(def input (read-string (str "[" (-> "day11/input" clojure.java.io/resource slurp) "]")))

(defn solve-1 []
  (let [[x y] (reduce #(mapv + %1 (directions %2)) [0 0] input)]
    (+ ^int (Math/abs ^int x) ^int (/ (- (Math/abs ^int y) (Math/abs ^int x)) 2))))

(defn solve-2 []
  (let [coords (reductions #(mapv + %1 (directions %2)) [0 0] input)]
    (apply max (map
                (fn [[x y]] (+ ^int (Math/abs ^int x)
                               ^int (/ (- (Math/abs ^int y) (Math/abs ^int x)) 2)))
                coords))))

(defn -main [& args]
  (time (println "part1: " (solve-1)))
  (time (println "part2: " (solve-2))))
