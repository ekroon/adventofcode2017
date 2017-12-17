(ns adventofcode.day17)

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def input 314)

(defn solve-1 [input ^long steps]
  (second
   (reduce
    (fn [b v] (let [split-position (inc ^int (mod input v))
                    [before after] (split-at split-position b)]
                (into [] (concat [v] after before))))
    [0] (range 1 (inc steps)))))

(defn solve-2 [input ^long steps]
  (:result
   (reduce
    (fn [{:keys [result current-position]} v]
      (let [next-position (inc ^int (mod (+ ^int current-position ^int input) v))]
        {:result (if (= 1 next-position) v result)
         :current-position next-position}))
    {:result 0
     :current-position 0}
    (range 1 (inc steps)))))

(defn generate-positions ^longs [^long skip-size ^long steps]
  (let [size   (inc steps)
        buffer (long-array size)]
    (aset buffer 0 0)
    (loop [current-position 0 step 1]
      (when (< step size)
        (let [next-position (inc ^long (mod (+ current-position skip-size) step))]
          (aset buffer step next-position)
          (recur next-position (inc step)))))
    buffer))

(defn solve [input ^long steps ^long target]
  (let [positions (generate-positions input steps)]
    (loop [position steps
           target-position (inc (aget ^longs positions target))]
      (when (>= position 0)
        (let [current-position (aget ^longs positions position)]
          (if (= current-position target-position)
            position
            (recur (dec position)
                   (if (< current-position target-position)
                     (dec target-position)
                     target-position))))))))

(defn -main [& args]
  (println "part-1a:" (time (solve input 2017 2017)))
  (println "part-2a:" (time (solve input 50000000 0)))
  (println "part-1b:" (time (solve-1 input 2017)))
  (println "part-2b:" (time (solve-2 input 50000000))))
