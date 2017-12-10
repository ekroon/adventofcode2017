(ns adventofcode.day10
  (:require [clojure.string :as str]))

(def input
  (-> "day10/input" clojure.java.io/resource slurp))

(defn reverse-first-n [n col]
  (let [[b e] (split-at n col)]
    (concat (reverse b) e)))

(defn rotate [n col]
  (let [[b e] (split-at n col)]
    (concat e b)))

(defn solve [parse-fn result-fn size input rounds]
  (let [input     (mapcat identity (repeat rounds (parse-fn input)))
        skip-list (range)]
    (let [rotated-result
          (reduce
           (fn [{:keys [list position]}
                [skip input prev-input]]
             (let [clist list]
               {:list     (->> list
                               (reverse-first-n input)
                               (rotate (mod (+ input skip) size)))
                :position (mod (+ position skip input) size)}))
           {:list     (range size)
            :position 0}
           (map #(vector %1 %2 %3)
                (range)
                input
                (concat [0] input)))]
      (result-fn
       (update rotated-result
               :list
               #(rotate (- size (:position rotated-result)) %))))))

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
