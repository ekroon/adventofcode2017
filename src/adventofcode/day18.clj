(ns adventofcode.day18
  (:require [clojure.java.io :as io]
            [clojure.set :as cs]
            [clojure.string :as str]))

(def input (-> "day18/input" io/resource slurp (str/split-lines)))
(def parsed-input (mapv #(read-string (str "[" % "]")) input))

(def example-input (-> "day18/example-input" io/resource slurp (str/split-lines)))
(def parsed-example-input (mapv #(read-string (str "[" % "]")) example-input))

(def state {:counter 0})

(defn get-value [state reg default]
  (if (number? reg)
    reg
    (get state reg default)))

(defn increase-counter [state]
  (update state :counter inc))

(defn snd-instruction [[reg1] state]
  (-> (assoc state :played-sound (get-value state reg1 0))
      increase-counter))

(defn set-instruction [[reg1 reg2] state]
  (-> (assoc state reg1 (get-value state reg2 0))
      increase-counter))

(defn add-instruction [[reg1 reg2] state]
  (-> (assoc state reg1 (+ (get-value state reg1 0) (get-value state reg2 0)))
      increase-counter))

(defn mul-instruction [[reg1 reg2] state]
  (-> (assoc state reg1 (* (get-value state reg1 0) (get-value state reg2 0)))
      increase-counter))

(defn mod-instruction [[reg1 reg2] state]
  (-> (assoc state reg1 (mod (get-value state reg1 0) (get-value state reg2 0)))
      increase-counter))

(defn rcv-instruction [[reg1] state]
  (if (not= 0 (get-value state reg1 0))
    (reduced (get-value state :played-sound 0))
    (increase-counter state)))

(defn jgz-instruction [[reg1 reg2] state]
  (if (< 0 (get-value state reg1 0))
    (update state :counter + reg2)
    (increase-counter state)))

(def config {'snd snd-instruction
             'set set-instruction
             'add add-instruction
             'mul mul-instruction
             'mod mod-instruction
             'rcv rcv-instruction
             'jgz jgz-instruction})

(defn run-instruction [config instructions state program-cycle]
  (let [instruction (nth instructions (get state :counter) nil)]
    (if instruction
      ((get config (first instruction)) (next instruction) state)
      (reduced state))))

(defn run [parsed-input]
  (reduce (partial run-instruction config parsed-input) state (range)))

(defn -main [& args]
  (println "part1:" (run parsed-input)))
