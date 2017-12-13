(ns adventofcode.day13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def input (-> "day13/input" io/resource slurp (str/split #"\n")))

(defn parse-line [line]
  (let [numbers (remove empty? (str/split line #"[^0-9]"))]
    [(read-string (first numbers))
     (read-string (second numbers))]))

(defn parse-lines [lines]
  (reduce (fn [r line] (let [[k v] (parse-line line)]
                         (conj r [k v]))) [] lines))

(defn caught-position [d r]
  (= 0 (mod d (- (* 2 ^int r) 2))))

(defn solve-1 [input]
  (let [parsed (parse-lines input)
        mapped (map (fn [[d r]] (if (caught-position d r)
                                  (* ^int d ^int r)
                                  0)) parsed)]
    (reduce + mapped)))

(defn caught [parsed delay]
  (reduce
    (fn [_ [d r]]
      (if (caught-position (+ ^int d ^int delay) r)
        (reduced true)
        false))
    false parsed))

(defn solve-2 [input]
  (let [parsed (parse-lines input)]
    (first (drop-while #(= :caught %)
                       (map (fn [d] (if-not (caught parsed d)
                                      d
                                      :caught))
                            (range 1000000000))))))

(defn -main [& args]
  (time (println "part1:" (solve-1 input)))
  (time (println "part2:" (solve-2 input))))