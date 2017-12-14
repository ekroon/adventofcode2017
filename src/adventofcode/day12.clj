(ns adventofcode.day12
  (:require [clojure.java.io :as io]
            [clojure.set :as cs]
            [clojure.string :as str]))

(def input (-> "day12/input" io/resource slurp (str/split #"\n")))

(defn parse-line [line]
  (let [numbers (remove empty? (str/split line #"[^0-9]"))]
    [(read-string (first numbers))
     (into [] (map read-string (next numbers)))]))

(defn parse-lines [lines]
  (reduce (fn [r line] (let [[k v] (parse-line line)]
                         (assoc r k v))) {} lines))

(defn solve [node-map start]
  (loop [unvisited (into #{} (keys node-map))
         distances {start 0}
         current start]
    (let [neighbours (node-map current [])
          distance (distances current)
          new-unvisited (disj unvisited current)]
      (if (nil? distance)
        distances
        (let [new-distances
              (reduce (fn [r v]
                        (update r v #(if % (min % (inc distance)) (inc distance))))
                      distances
                      (filter new-unvisited neighbours))
              new-current
              (ffirst (sort-by second
                               (remove (comp nil? second)
                                       (map #(vector % (new-distances %))
                                            new-unvisited))))]
          (recur new-unvisited
                 new-distances
                 new-current))))))

(defn solve-1 [node-map]
  (-> (solve node-map 0) vals count))

(defn solve-2 [node-map]
  (let [result (reduce (fn [r v]
                         (if (or ((:seen r) v) (nil? (get node-map v)))
                           r
                           (let [new
                                 (-> r
                                     (update :seen cs/union
                                             (into #{} (keys (solve node-map v))))
                                     (update :groups inc))]
                             new)))
                       {:seen   #{}
                        :groups 0}
                       (keys node-map))]
    (:groups result)))

(defn -main [& args]
  (let [node-map (parse-lines input)]
    (time (println "part1: " (solve-1 node-map)))
    (time (println "part2: " (solve-2 node-map)))))
