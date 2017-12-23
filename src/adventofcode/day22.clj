(ns adventofcode.day22
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def input (-> "day22/input" io/resource slurp (str/split-lines)))

(def sample-input (-> "day22/sample-input" io/resource slurp (str/split-lines)))

(defn parse-input [input]
  (into {}
        (remove nil?
                (for [x (range (count (first input)))
                      y (range (count input))]
                  (when (= \# (nth (nth input y) x))
                    [[x y] true])))))

(defn left [[^long x ^long y]]
  [y (- ^long x)])

(defn right [[x y]]
  [(- ^long y) x])

(defn solve-1 [input]
  (let [infected (parse-input input)]
    (:new-infected
     (reduce
      (fn [{:keys [current direction infected new-infected]} _]
        (let [status (get infected current false)
              new-direction (if status (right direction) (left direction))]
          {:current (mapv + current new-direction)
           :direction new-direction
           :infected (if status (dissoc infected current) (assoc infected current true))
           :new-infected (if status new-infected (inc ^long new-infected))}))
      {:current [(int (/ (count input) 2))
                 (int (/ (count input) 2))]
       :direction [0 -1]
       :infected infected
       :new-infected 0}
      (range 10000)))))

(def transition {true :flagged
                 nil :weakened
                 :weakened :infected
                 :infected :flagged
                 :flagged :cleaned})

(defn reverse' [[x y]]
  [(- ^long x) (- ^long y)])

(def direction-change {nil left
                       :clean left
                       :weakened identity
                       :infected right
                       true right
                       :flagged reverse'})

(defn solve-2 [input]
  (:new-infected
   (reduce
    (fn [{:keys [current direction infected new-infected]} counter]
      (when (= 0 (mod counter 500000))
        (print (format "cycles: %s/%s\r" counter 10000000)))
      (let [status (get infected current nil)
            new-direction ((direction-change status) direction)
            new-status (transition status)]
        {:current (mapv + current new-direction)
         :direction new-direction
         :infected (if (= new-status :cleaned)
                     (dissoc infected current)
                     (assoc infected current new-status))
         :new-infected (if (= :infected new-status) (inc ^long new-infected) new-infected)}))
    {:current [(long (/ (count input) 2))
               (long (/ (count input) 2))]
     :direction [(long 0) (long -1)]
     :infected (parse-input input)
     :new-infected 0}
    (range 10000000))))

(defn -main [& args]
  (println "part1:" (time (solve-1 input)))
  (println "part2:" (time (solve-2 sample-input))))
