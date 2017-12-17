(ns adventofcode.day17)

;; (set! *warn-on-reflection* true)
;; (set! *unchecked-math* :warn-on-boxed)

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

(defn -main [& args]
  (println "part-1:" (time (solve-1 input 2017)))
  (println "part-2:" (time (solve-2 input 50000000))))
