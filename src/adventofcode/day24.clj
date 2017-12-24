(ns adventofcode.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse [s]
  (map #(into [] (map read-string (str/split % #"/"))) s))

(def input (-> "day24/input" io/resource slurp (str/split #"\n") parse))

(def example-input (-> "day24/example-input" io/resource slurp (str/split #"\n") parse))

(defn matches [parts connect-with]
  (filter (fn [[from to]] (or (= connect-with from) (= connect-with to))) parts))

(defn step [nodes current connected-with path]
  (let [new-nodes  (disj nodes current)
        connect-to (if (= connected-with (first current)) (second current) (first current))
        matching   (matches new-nodes connect-to)]
    (if (seq matching)
      (reduce max (map #(step new-nodes % connect-to (conj path current)) matching))
      (reduce + (map (partial reduce +) (conj path current))))))

(defn solve-1 [input]
  (let [nodes    (into #{} input)
        starting (matches nodes 0)]
    (reduce max (map #(step nodes % 0 []) starting))))

#_(solve-1 input)


(defn -main [& args]
  (println (time (solve-1 input))))
