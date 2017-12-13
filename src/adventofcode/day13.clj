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

(defn scanner-position [d r]
  ((fn [d r]
     (let [period (concat (range r)
                          (range (dec ^int r) 1 -1))]
       (->> (cycle period)
            (drop d)
            first)))
   (mod d (+ ^int r (max 0 (- ^int r 2)))) r))

(defn solve-1 [input]
  (let [parsed (parse-lines input)
        mapped (map (fn [[d r]] (if (= 0 (scanner-position
                                          d r))
                                  (* ^int d ^int r)
                                  0)) parsed)]
    (reduce + mapped)))

(defn caught [parsed delay]
  (reduce
   (fn [_ [d r]]
     (if (= 0 (scanner-position (+ ^int d ^int delay) r))
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
