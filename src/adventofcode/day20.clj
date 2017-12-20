(ns adventofcode.day20
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; p=<-11104,1791,5208>, v=<-6,36,-84>, a=<19,-5,-4>
(def input (->> "day20/input" io/resource slurp))

(def parsed-input (re-seq #"p=<([-0-9]*),([-0-9]*),([-0-9]*)>, v=<([-0-9]*),([-0-9]*),([-0-9]*)>, a=<([-0-9]*),([-0-9]*),([-0-9]*)>" input))

(defn abs ^long [^long v]
  (if (pos? v)
    v
    (- v)))

(defn solve-1 []
  (ffirst (sort-by
           (juxt #(nth % 3) #(nth % 2) #(nth % 1))
           (map-indexed
            (fn [i [line x y z vx vy vz ax ay az]]
              [i
               (+ (abs (read-string x))
                  (abs (read-string y))
                  (abs (read-string z)))
               (+ (abs (read-string vx))
                  (abs (read-string vy))
                  (abs (read-string vz)))
               (+ (abs (read-string ax))
                  (abs (read-string ay))
                  (abs (read-string az)))
               line])
            parsed-input))))

(def particle {:id           0
               :position     [0 0 0]
               :velocity     [0 0 0]
               :acceleration [0 0 0]})

(def particles (map-indexed
                (fn [i [line x y z vx vy vz ax ay az]]
                  {:id i
                   :position [(read-string x) (read-string y) (read-string z)]
                   :velocity [(read-string vx) (read-string vy) (read-string vz)]
                   :acceleration [(read-string ax) (read-string ay) (read-string az)]})
                parsed-input))

(defn remove-collisions [particles]
  (as-> particles $
        (group-by :position $)
        (remove (fn [[_ particles]] (< 1 (count particles))) $)
        (mapcat val $)))

(defn step [particle]
  (let [velocity (mapv + (:velocity particle) (:acceleration particle))
        position (mapv + (:position particle) velocity)]
    (-> particle
        (assoc :velocity velocity)
        (assoc :position position))))

(defn abs-distance [xyz]
  (reduce + (map abs xyz)))

(defn solve-2 []
  (let [particle-steps (iterate (fn [particles]
                                  (sort-by (juxt #(abs-distance (:acceleration %))
                                                 #(abs-distance (:velocity %))
                                                 #(abs-distance (:position %)))
                                           (remove-collisions
                                            (map step particles))))
                                particles)]
    (count (reduce (fn [r v]
                     (let [ids (map :id v)]
                       (if (= r ids) (reduced ids) ids)))
                   [] particle-steps))))

(defn -main [& args]
  (println "part1:" (time (solve-1)))
  (println "part2:" (time (solve-2))))
