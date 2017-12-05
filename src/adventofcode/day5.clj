(ns adventofcode.day5)

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn input []
  (clojure.java.io/reader (clojure.java.io/resource "day5/input")))

(defn example-part1 []
  (clojure.java.io/reader (clojure.java.io/resource "day5/example-part1")))

(defn str->number [str]
  (Integer/parseInt str))

(defn calculate-next ^long [update-fn ^long index ^longs state]
  (let [next-index (+ index (aget state index))
        value      (update-fn (aget state index))]
    (aset state index ^int value)
    next-index))

(defn solve [reader update-fn]
  (let [^longs state (long-array (mapv str->number (line-seq reader)))
        exit         ^int (alength state)]
    (loop [index 0
           steps 0]
      (let [next-index (calculate-next update-fn index state)]
        (if (< next-index exit)
          (recur next-index (inc steps))
          (inc steps))))))

(defn solve-1 [reader]
  (solve reader inc))

(defn solve-2 [reader]
  (solve reader (fn [^long v] (if (>= v 3) (dec v) (inc v)))))

(defn -main [& args]
  (time (println "part1: " (solve-1 (input))))
  (time (println "part2: " (solve-2 (input)))))
