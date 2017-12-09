(ns adventofcode.day9)

(def input
  (-> "day9/input" clojure.java.io/resource slurp))

(def begin-state {:depth 0
                  :total 0
                  :total-collected 0
                  :ignore-next false
                  :garbage false})

(defn next-state [state c]
  (cond
    (and (:garbage state) (:ignore-next state))
    (assoc state :ignore-next false)

    (and (:garbage state) (= \! c))
    (assoc state :ignore-next true)

    (and (:garbage state) (not= \> c))
    (update state :total-collected inc)

    (and (:garbage state) (= \> c))
    (assoc state :garbage false)

    (= \< c)
    (assoc state :garbage true)

    (= \{ c)
    (update state :depth inc)

    (= \} c)
    (-> state
        (update :total + (:depth state))
        (update :depth dec))

    :else
    state))

(defn solve [input]
  (reduce next-state begin-state input))

;; (solve "{{<a!>},{<a!>},{<a!>},{<ab>}}")
;; (solve "{{<!!>},{<!!>},{<!!>},{<!!>}}")

(defn -main [& args]
  (let [{:keys [total total-collected]} (solve input)]
    (println "part1: " total)
    (println "part2: " total-collected)))
