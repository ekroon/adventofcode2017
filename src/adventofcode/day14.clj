(ns adventofcode.day14
  (:require [adventofcode.day10 :refer [solve parse-2 result-2]]))

(def input (mapv #(str "jxqlasbh" "-" %) (range 128)))

(defn knot-hash [input] (solve parse-2 result-2 256 input 64))

(def hashes (mapv knot-hash input))

(defn hex->binary-vec [c]
  (Integer/toBinaryString (Integer/parseInt (str c) 16)))

(defn count-1 [hex-string]
  (reduce + (map #(-> (hex->binary-vec %)
                      (frequencies)
                      (get \1 0)) hex-string)))

(def part1 (reduce + (map count-1 hashes)))

