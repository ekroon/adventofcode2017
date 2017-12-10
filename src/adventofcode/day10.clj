(ns adventofcode.day10
  (:require [clojure.string :as str]))

(def input
  (-> "day10/input" clojure.java.io/resource slurp))

(def make-indexes
  (memoize
   (fn [size position input]
     (as-> (cycle (range size)) $
       (drop position $)
       (take size (concat (reverse (take input $)) (drop input $)))
       (drop (- size position) (cycle $))
       (take size $)))))

(defn rearrange-list [list indexes]
  (mapv (fn [i] (nth list i)) indexes))

(defn solve [parse-fn result-fn size input rounds]
  (let [input     (mapcat identity (repeat rounds (parse-fn input)))
        skip-list (range)]
    (result-fn
     (reduce
      (fn [{:keys [list position]}
           [skip input prev-input]]
        (let [clist (cycle list)]
          {:list     (rearrange-list list (make-indexes size position input))
           :position (mod (+ position skip input) size)}))
      {:list     (range size)
       :position 0}
      (map #(vector %1 %2 %3)
           (range)
           input
           (concat [0] input))))))

(defn parse-1 [str1]
  (map read-string (str/split str1 #",")))

(defn result-1 [c]
  (* (first (:list c))
     (second (:list c))))

(defn parse-2 [str1]
  (concat (.getBytes str1) [17, 31, 73, 47, 23]))

(defn result-2 [c]
  (let [blocks (partition 16 (:list c))
        xblocks (map (fn [b] (apply bit-xor b)) blocks)
        sblocks (map (fn [x] (format "%02x" x)) xblocks)]
    (apply str sblocks)))

(defn -main [& args]
  (let [part1 (solve parse-1 result-1 256 input 1)
        part2 (time (solve parse-2 result-2 256 input 64))]
    (println "part1: " part1)
    (println "part2: " part2)))
