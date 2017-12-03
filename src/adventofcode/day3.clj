(ns adventofcode.day3)

(def directions [[1 0][0 -1][-1 0][0 1]])

(defn abs [v]
  (max v (- v)))

(defn steps []
  (let [r (interleave (range 1 Long/MAX_VALUE)
                      (range 1 Long/MAX_VALUE))
        s (mapcat (fn [a d] (repeat a d)) r (cycle directions))]
    s))

(defn solve-1 [input]
  (let [amount (- input 1)
        [x y] (reduce (fn [r v] (mapv + r v)) (take amount (steps)))]
    (+ (abs x) (abs y))))

(defn update-matrix [matrix current]
  (let [[x y] current
        value (reduce +
                      (remove nil? (for [x1 (range -1 2) y1 (range -1 2)]
                                     (get-in matrix [(+ x x1) (+ y y1)]))))]
    [value (assoc-in matrix current value)]))

(defn solve-2 [input]
  (let [size   (Math/ceil (Math/sqrt input))
        matrix (into [] (repeat size (into [] (repeat size 0))))
        start  [(int (- (Math/ceil (/ size 2)) 1)) (int (- (Math/floor (/ size 2)) 0))]]
    (loop [matrix (assoc-in matrix start 1)
           coord  start
           steps  (steps)]
      (let [current-step        (first steps)
            next-coord          (mapv + coord current-step)
            [value next-matrix] (update-matrix matrix next-coord)]
        (if (<= value input)
          (recur next-matrix next-coord (drop 1 steps))
          value)))))

;; (solve-1 368078)
;; (solve-2 368078)

(defn -main [& args]
  (println "part1: " (solve-1 368078))
  (println "part2: " (solve-2 368078)))
