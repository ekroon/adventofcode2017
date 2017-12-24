(ns adventofcode.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse [s]
  (map #(into [] (map read-string (str/split % #"/"))) s))

(def input (-> "day24/input" io/resource slurp (str/split #"\n") parse))

(def example-input (-> "day24/example-input" io/resource slurp (str/split #"\n") parse))

(defn matches [parts connect-with]
  (filter (fn [[from to]] (or (= connect-with from) (= connect-with to))) parts))

(defn step [compare-fn nodes current connected-with path]
  (let [new-nodes  (disj nodes current)
        connect-to (if (= connected-with (first current)) (second current) (first current))
        matching   (matches new-nodes connect-to)]
    (if (seq matching)
      (reduce
       compare-fn
       (map #(step compare-fn new-nodes % connect-to (conj path current)) matching))
      [(count (conj path current))
       (reduce + (map (partial reduce +) (conj path current)))])))

(defn solve [compare-fn input]
  (let [nodes    (into #{} input)
        starting (matches nodes 0)]
    (second
     (reduce compare-fn (map #(step compare-fn nodes % 0 []) starting)))))

(defn compare-part-1
  [[_ max-weight]
   [_ weight]]
  [0 (max max-weight weight)])

(defn compare-part-2
  [[max-length
    max-weight]
   [length
    weight]]
  (cond
    (> length max-length) [length weight]
    (= length max-length) [length (max weight max-weight)]
    :else [max-length max-weight]))

(defn -main [& args]
  (println "part1:" (time (solve compare-part-1 input)))
  (println "part2:" (time (solve compare-part-2 input))))
