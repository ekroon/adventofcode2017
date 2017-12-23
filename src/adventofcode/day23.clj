(ns adventofcode.day23
  (:require [clojure.java.io :as io]
            [clojure.set :as cs]
            [clojure.string :as str]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def input (-> "day23/input" io/resource slurp (str/split-lines)))
(def parsed-input (mapv #(read-string (str "[" % "]")) input))

(def state {:counter 0
            :mul-instruction 0})

(defn get-value [state reg default]
  (if (number? reg)
    reg
    (get state reg default)))

(defn increase-counter [state]
  (update state :counter inc))

(defn set-instruction [[reg1 reg2] state]
  (-> (assoc state reg1 (get-value state reg2 0))
      increase-counter))

(defn sub-instruction [[reg1 reg2] state]
  (-> (assoc state reg1 (- ^long (get-value state reg1 0) ^long (get-value state reg2 0)))
      increase-counter))

(defn mul-instruction [[reg1 reg2] state]
  (-> (assoc state reg1 (* ^long (get-value state reg1 0) ^long (get-value state reg2 0)))
      (update :mul-instruction inc)
      increase-counter))

(defn jnz-instruction [[reg1 reg2] state]
  (if (not (zero? ^long (get-value state reg1 0)))
    (update state :counter + (get-value state reg2 0))
    (increase-counter state)))

(def config-1
  {'set set-instruction
   'sub sub-instruction
   'mul mul-instruction
   'jnz jnz-instruction})

(defn run-instruction [config instructions state program-cycle]
  (let [instruction (nth instructions (get state :counter) nil)]
    (when (= 0 (mod program-cycle 10000000))
      (println state))
    (if instruction
      ((get config (first instruction)) (next instruction) state)
      (reduced state))))

(defn run-1 [parsed-input]
  (:mul-instruction (reduce (partial run-instruction config-1 parsed-input) state (range))))

#_(run-1 parsed-input)

(defn -main [& args]
  (println "part1:" (run-1 parsed-input)))
