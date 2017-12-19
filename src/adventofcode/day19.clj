(ns adventofcode.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> "day19/input" io/resource slurp str/split-lines (mapv #(mapv identity %))))
(def example-input (->> "day19/example-input" io/resource slurp str/split-lines (mapv #(mapv identity %))))

;; x y
(def coord [0 0])

(defn find-start [input-line]
  (let [x (some identity (map-indexed #(when (= \| %2) %1) input-line))]
    [x 0]))

(def state {:position [0 0]
            :direction :down
            :letters []})

(def position-change {:down  [0 1]
                      :up    [0 -1]
                      :left  [-1 0]
                      :right [1 0]})

(def direction-char {:down   \|
                     :up     \|
                     :left   \-
                     :right  \-
                     :change \+
                     :empty  (char \ )})

(def filter-chars #{(char \ ) \| \- \+})

(defn char-at [grid [x y]]
  (nth (nth grid y) x))

(defn direction-filter [grid current-position direction]
  (let [new-position (mapv + current-position (position-change direction))]
    (when (not= (char-at grid new-position)
                (direction-char :empty))
      [new-position direction])))

(defn change-direction [grid current-position current-direction]
  (let [current-char (char-at grid current-position)]
    (cond
      (#{:up :down} current-direction)
      (some identity
            (map (partial direction-filter grid current-position)
                 [:left
                  :right]))
      (#{:left :right} current-direction)
      (some identity
            (map (partial direction-filter grid current-position)
                 [:up
                  :down]))
      :else (assert false))))

(defn next-position-and-direction [grid current-position current-direction]
  (let [current-char (char-at grid current-position)]
    (cond
      (= current-char (direction-char :change))
      (change-direction grid current-position current-direction)

      (not= (direction-char :empty) current-char)
      [(mapv + current-position (position-change current-direction))
       current-direction]

      :else (reduced [current-position :invalid]))))

(defn solve [input]
  (let [start     (find-start (first input))
        direction :down
        grid      input
        next      (partial next-position-and-direction grid)
        char-at   (partial char-at grid)]
    (reduce (fn [{:keys [position direction letters]} steps]
              (let [next-result (next position direction)]
                (if (reduced? next-result)
                  (reduced {:position (first @next-result)
                            :direction (second @next-result)
                            :letters letters
                            :steps steps})
                  (let [[next-position next-direction] next-result]
                    {:position next-position
                     :direction next-direction
                     :letters (if (filter-chars (char-at position))
                                letters
                                (conj letters (char-at position)))}))))
            (assoc state :position start)
            (range))))

(defn -main [& args]
  (let [solution (time (solve input))]
    (println "part1:" (apply str (:letters solution)))
    (println "part2:" (:steps solution))))
