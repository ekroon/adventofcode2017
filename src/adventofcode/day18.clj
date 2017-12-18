(ns adventofcode.day18
  (:require [clojure.java.io :as io]
            [clojure.set :as cs]
            [clojure.string :as str]))

(def input (-> "day18/input" io/resource slurp (str/split-lines)))
(def parsed-input (mapv #(read-string (str "[" % "]")) input))

(def example-input (-> "day18/example-input" io/resource slurp (str/split-lines)))
(def parsed-example-input (mapv #(read-string (str "[" % "]")) example-input))

(def example-input-2 (-> "day18/example-input-2" io/resource slurp (str/split-lines)))
(def parsed-example-input-2 (mapv #(read-string (str "[" % "]")) example-input-2))

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
    (update state :counter + (get-value state reg2 0))
    (increase-counter state)))

(def config-1
  {'snd snd-instruction
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

(defn run-1 [parsed-input]
  (reduce (partial run-instruction config-1 parsed-input) state (range)))

(defn receive-instruction [[reg1] state]
  (if (seq (get state :message-queue))
    (let [value (first (get state :message-queue))
          queue (subvec (get state :message-queue) 1)]
      (-> state
          (assoc reg1 value)
          (assoc :message-queue queue)
          increase-counter))
    (assoc state :can-progress false)))


(defn send-instruction [[reg1] state]
  (-> state
      (assoc :message (get-value state reg1 0))
      (update :send-counter inc)
      increase-counter))

(def config-2
  {'snd send-instruction
   'set set-instruction
   'add add-instruction
   'mul mul-instruction
   'mod mod-instruction
   'rcv receive-instruction
   'jgz jgz-instruction})

(defn copy-messages [from to]
  (if (:message from)
    [(dissoc from :message)
     (-> to
         (update :message-queue conj (:message from))
         (assoc :can-progress true))]
    [from to]))

(defn run-2 [parsed-input]
  (let [state {0 {:counter 0
                  :program-id 0
                  'p       0
                  :can-progress true
                  :message-queue []
                  :send-counter 0}
               1 {:counter 0
                  :program-id 1
                  'p       1
                  :can-progress true
                  :message-queue []
                  :send-counter 0}}
        run-instruction (partial run-instruction config-2 parsed-input)]
    (reduce (fn [state program-cycle]
              (cond
                (and (not (reduced? (get state 0)))
                     (get-in state [0 :can-progress]))
                (let [state0 (run-instruction (get state 0) program-cycle)
                      [state0 state1] (copy-messages state0 (get state 1))]
                  {0 state0 1 state1})
                (and (not (reduced? (get state 1)))
                     (get-in state [1 :can-progress]))
                (let [state1 (run-instruction (get state 1) program-cycle)
                      [state1 state0] (copy-messages state1 (get state 0))]
                  {0 state0 1 state1})
                :else (reduced (get-in state [1 :send-counter]))))
            state (range))))

(defn -main [& args]
  (println "part1:" (run-1 parsed-input))
  (println "part2:" (run-2 parsed-input)))
