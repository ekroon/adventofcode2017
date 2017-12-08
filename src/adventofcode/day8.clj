(ns adventofcode.day8
  (:require [clojure.string :as str]))

(defn input []
  (clojure.java.io/reader (clojure.java.io/resource "day8/input")))

(defn example-part1 []
  (clojure.java.io/reader (clojure.java.io/resource "day8/example-part1")))

;;  0 1   2    3 4 5  6
;; (c inc -20 if c == 10)

(defn parse-instruction [line]
  (let [tokens (str/split line #" ")]
    (map read-string tokens)))

(def lookup {'< < '> > '<= <= '>= >= '== == '!= not= 'inc + 'dec -})

(defn never [& args])

(defn run-instructions [state instructions]
  (reductions (fn [s i]
                (if ((get lookup (nth i 5) never)
                     (or (get s (nth i 4)) 0)
                     (nth i 6))
                  (update s (nth i 0) (fnil (get lookup (nth i 1)) 0) (nth i 2))
                  s))
              state instructions))

(defn solve [reader-fn]
  (let [lines  (map parse-instruction (line-seq (reader-fn)))
        states (run-instructions {} lines)]
    [(->> states last vals (apply max))
     (->> (mapcat vals states) (apply max))]))

(defn -main [& args]
  (let [[part1 part2] (solve input)]
    (println "part1: " part1)
    (println "part2: " part2)))
