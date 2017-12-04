(ns adventofcode.day3)

(def directions [[1 0][0 -1][-1 0][0 1]])

(defn steps []
  (let [r (interleave (range 1 Long/MAX_VALUE)
                      (range 1 Long/MAX_VALUE))
        s (mapcat repeat r (cycle directions))]
    s))

(defn solve-1 [input]
  (let [amount (- input 1)
        [x y] (reduce (fn [r v] (mapv + r v)) (take amount (steps)))]
    (+ (Math/abs x) (Math/abs y))))

(defn update-matrix [matrix current]
  (let [[x y] current
        value (reduce +
                      (remove nil? (for [x1 (range -1 2) y1 (range -1 2)]
                                     (get-in matrix [(+ x x1) (+ y y1)]))))]
    [value (assoc-in matrix current value)]))

(defn solve-2 [input]
  (let [matrix {[0 0] 1}
        start  [0 0]]
	(->> (reductions (fn [[coord value matrix] current-step] 
	                (let [next-coord (mapv + coord current-step)
					      [value next-matrix] (update-matrix matrix next-coord)]
				      [next-coord value next-matrix]))
				[start 1 matrix]
			    (steps))
       (drop-while (fn [[_ value _]] (<= value input)))
       first
       second)))

;; (solve-1 368078)
;; (solve-2 368078)

(defn -main [& args]
  (println "part1: " (solve-1 368078))
  (println "part2: " (solve-2 368078)))
