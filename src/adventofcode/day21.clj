(ns adventofcode.day21
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def input (-> "day21/input" io/resource slurp str/split-lines))

(defn rotate2 [strs]
  [(apply str [(get-in strs [1 0]) (get-in strs [0 0])])
   (apply str [(get-in strs [1 1]) (get-in strs [0 1])])])

(defn rotate3 [strs]
  [(apply str [(get-in strs [2 0]) (get-in strs [1 0]) (get-in strs [0 0])])
   (apply str [(get-in strs [2 1]) (get-in strs [1 1]) (get-in strs [0 1])])
   (apply str [(get-in strs [2 2]) (get-in strs [1 2]) (get-in strs [0 2])])])

(defn rotate [strs]
  (if (= 2 (count (first strs)))
    (rotate2 strs)
    (rotate3 strs)))

(defn flip [strs]
  (mapv #(apply str (reverse %)) strs))

(defn parse [line]
  (let [[in out]    (str/split line #" => ")
        in-pattern  (str/split in #"/")
        out-pattern (str/split out #"/")]
    (as-> {} $
          (assoc $ (-> in-pattern) out-pattern)
          (assoc $ (-> in-pattern flip) out-pattern)
          (assoc $ (-> in-pattern rotate) out-pattern)
          (assoc $ (-> in-pattern rotate flip) out-pattern)
          (assoc $ (-> in-pattern rotate rotate) out-pattern)
          (assoc $ (-> in-pattern rotate rotate flip) out-pattern)
          (assoc $ (-> in-pattern rotate rotate rotate) out-pattern)
          (assoc $ (-> in-pattern rotate rotate rotate flip) out-pattern))))

(def rules (into {} (mapcat parse input)))

(def start [".#." "..#" "###"])

(defn split-strs [strs ^long size]
  (into []
        (for [y (range (/ (count strs) size))
              x (range (/ (count strs) size))]
          (let [rows (take size (drop (* ^long y size) strs))]
            (mapv #(apply str (take size (drop (* ^long x size) %))) rows)))))

(defn join-strs [strs]
  (let [size  (int (Math/sqrt (count strs)))
        steps (partition size strs)]
    (into []
          (mapcat (fn [vecs] (apply mapv (fn [& args] (apply str args)) vecs)) steps))))

(defn solve-1 [i]
  (let [steps (range i)
        strs
        (reduce
         (fn [strs _]
           (let [size (if (= 0 (mod (count strs) 2)) 2 3)]
             (as-> strs $
               (split-strs $ size)
               (map #(get rules %) $)
               (join-strs $))))
         start
         steps)]
    (reduce + (map #(count (filter #{\#} %)) strs))))

(defn -main [& args]
  (println "part1:" (time (solve-1 5)))
  (println "part2:" (time (solve-1 18))))
