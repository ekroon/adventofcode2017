(ns adventofcode.day14
  (:require [clojure.string :as str]
            [adventofcode.day10 :refer [solve parse-2 result-2]]
            [adventofcode.day12 :refer [solve-2]]))

(def results
  (delay
    (def input (mapv #(str "jxqlasbh" "-" %) (range 128)))

    (defn knot-hash [input] (solve parse-2 result-2 256 input 64))

    (def hashes (mapv knot-hash input))

    (defn hex->binary-vec [c]
      (str/replace (format "%4s" (Integer/toBinaryString (Integer/parseInt (str c) 16)))
                   #" " "0"))

    (defn count-1 [hex-string]
      (reduce + (map #(-> (hex->binary-vec %)
                          (frequencies)
                          (get \1 0)) hex-string)))

    (def part1 (reduce + (map count-1 hashes)))

    (println "part1:" part1)

    (def binary-vecs
      (mapv
        (fn [s] (mapv #(if (= \1 %) 1 0) (mapcat hex->binary-vec s)))
        hashes))

    (doall (mapv #(str/replace (apply str %)
                               #"0" " ") binary-vecs))

    (defn get-around [x y]
      [[(inc x) y]
       [(dec x) y]
       [x (inc y)]
       [x (dec y)]])

    (defn make-node-map [binary-vecs]
      (let [size (count (first binary-vecs))]
        (into (sorted-map)
              (remove nil?
                      (for [row (range size)
                            col (range size)]
                        (when-not (zero? (get-in binary-vecs [row col]))
                          [(inc (+ (* row size) col)),
                           (into [] (remove zero?
                                            (map (fn [[x y]] (* (get-in binary-vecs [x y] 0)
                                                                (inc (+ (* x size) y))))
                                                 (get-around row col))))]))))))

    (def part2 (time (solve-2 (make-node-map binary-vecs))))

    (println "part2:" part2)))

(defn -main [& args]
  @results)
