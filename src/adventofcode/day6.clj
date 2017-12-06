(ns adventofcode.day6)

(def input [11 11 13 7 0 15 5 5 4 4 1 1 7 1 15 11])
;; (def input [0 2 7 0])

(defn highest-element [input]
  (->> (map-indexed #(vector %1 %2) input)
       (sort-by second #(compare %2 %1))
       (first)))

;; (highest-element [0 2 7 0 7 2])

(defn create-overlay [size start-at amount]
  (let [overlay (into [] (repeat size (int (/ amount size))))
        overlay (into [] (map + overlay (concat
                                         (repeat (mod amount size) 1)
                                         (repeat 0))))
        overlay (into [] (take size (drop (- size start-at) (cycle overlay))))
        ]
    overlay))

;; (create-overlay 4 2 7)

(defn distribute-blocks [input]
  (let [size               (count input)
        [replace-at value] (highest-element input)
        overlay            (create-overlay size
                                           (mod (inc replace-at) size)
                                           value)]
    (into [] (map + (assoc input replace-at 0) overlay))))

;; (distribute-blocks [1 3 4 1])

(defn solve [input]
  (loop [current input
         seen    #{}
         steps   0]
    (if (seen current)
      [current steps]
      (recur (distribute-blocks current)
             (conj seen current)
             (inc steps)))))



(defn solve-1 [input]
  (let [[_ steps] (solve input)]
    steps))

(defn solve-2 [input]
  (let [[result _ ] (solve input)
        [_ steps]   (solve result)]
    steps))

(defn -main [& args]
  (println "part1: " (solve-1 input))
  (println "part2: " (solve-2 input)))
