(ns adventofcode.day25)

(defn left [state]
  (-> state
      (update :slot dec)))

(defn right [state]
  (-> state
      (update :slot inc)))

(defn one [{:keys [slot] :as state}]
  (-> state
      (assoc-in [:tape slot] 1)))

(defn zero [{:keys [slot] :as state}]
  (-> state
      (update :tape dissoc slot)))

(defn new-value [state value]
  (-> state
      (assoc :value value)))

(defn solve-1 []
  (count
   (:tape
    (reduce
     (fn [{:keys [slot value tape] :as state} cycle]
       (let [current (get tape slot 0)]
         (cond
           (= value :a) (if (= current 0)
                          (-> state one right (new-value :b))
                          (-> state zero left (new-value :c)))
           (= value :b) (if (= current 0)
                          (-> state one left (new-value :a))
                          (-> state one right (new-value :c)))
           (= value :c) (if (= current 0)
                          (-> state one right (new-value :a))
                          (-> state zero left (new-value :d)))
           (= value :d) (if (= current 0)
                          (-> state one left (new-value :e))
                          (-> state one left (new-value :c)))
           (= value :e) (if (= current 0)
                          (-> state one right (new-value :f))
                          (-> state one right (new-value :a)))
           (= value :f) (if (= current 0)
                          (-> state one right (new-value :a))
                          (-> state one right (new-value :e)))
           :else (assert false))))
     {:slot 0
      :value :a
      :tape {}}
     (range 12261543)))))

(defn -main [& args]
  (println "solution:" (time (solve-1))))
