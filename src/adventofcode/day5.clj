(ns adventofcode.day5)

(defn input []
  (clojure.java.io/reader (clojure.java.io/resource "day5/input")))

(defn example-part1 []
  (clojure.java.io/reader (clojure.java.io/resource "day5/example-part1")))

(defn str->number [str]
  (Integer/parseInt str))

(defn calculate-next [index state update-fn]
  (if (>= index (count state))(println [index (count state)]))
  (let [next-index (+ index (nth state index))
        next-state  (update state index update-fn)]
    [next-index next-state]))

(defn solve [reader update-fn]
  (println "------")
  (let [state (mapv str->number (line-seq reader))
        exit  (count state)]
    (println exit)
    (loop [index 0
           state state
           steps 0]
      (let [[next-index next-state] (calculate-next index state update-fn)]
        (if (and (< steps 30000000) (< next-index exit))
          (do
            (when (= 0 (mod steps 1000000)) (println "steps: " steps))
            (recur next-index next-state (inc steps)))
          (inc steps))))))

(defn solve-1 [reader]
  (solve reader inc))

(defn solve-2 [reader]
  (solve reader (fn [v] (if (>= v 3) (dec v) (inc v)))))

(defn -main [& args]
  (println "part1: " (solve-1 (input)))
  (println "part2: " (solve-2 (input))))
