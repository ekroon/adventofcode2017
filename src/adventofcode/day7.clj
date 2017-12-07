(ns adventofcode.day7)

(defn input []
  (clojure.java.io/reader (clojure.java.io/resource "day7/input")))

(defn example-part1 []
  (clojure.java.io/reader (clojure.java.io/resource "day7/example-part1")))

(defn str->map [str]
  (let [parsed (map second (re-seq #"(\w+)+" str))]
    {:name (keyword (first parsed))
     :weight (Integer/parseInt (second parsed))
     :supporting (into [] (map keyword (drop 2 parsed)))}))

(defn update-supported-by [structure list-item]
  (reduce
   (fn [r v] (assoc-in r [v :supported-by] (:name list-item)))
   (update-in structure [(:name list-item)] merge list-item)
   (:supporting list-item)))

(defn update-total-weight [structure list-item]
  (let [weights (map (fn [k] (get-in structure [k :total-weight]))
                     (:supporting (get structure list-item)))]
    (-> structure
        (update-in [list-item]
                   assoc :total-weight (+ (get-in structure [list-item :weight])
                                          (reduce + weights)))
        (update-in [list-item] assoc :weights weights))))

(defn parse-structure [reader]
  (let [lines (map str->map (line-seq reader))]
    (reduce update-supported-by {} lines)))

(defn solve-1 [reader]
  (let [structure (parse-structure reader)]
    (-> (filter (fn [[k v]] (not (:supported-by v))) structure)
        first first)))

(defn unequal-col [col]
  (if (empty? col)
    false
    (apply not= col)))

(defn line-ordering [structure start-at]
  (loop [lines []
         backlog [start-at]]
    (let [first-item (first backlog)
          rest-items (rest backlog)]
      (if first-item
        (recur (conj lines first-item) (concat rest-items
                                               (:supporting (first-item structure))))
        (into [] (reverse lines))))))

(defn calculate-value [structure supporting weights]
  (let [incorrect-value (-> (filter (fn [[k v]] (= 1 v)) (frequencies weights))
                            first first)
        correct-value   (-> (filter (fn [[k v]] (not= 1 v)) (frequencies weights))
                            first first)
        index           (.indexOf weights incorrect-value)
        incorrect-item  (get structure (nth supporting index))]
    (- (:weight incorrect-item) (- incorrect-value correct-value))))

(defn solve-2 [reader-fn]
  (let [structure (parse-structure (reader-fn))
        start-at  (solve-1 (reader-fn))
        lines      (line-ordering structure start-at)
        structure (reduce update-total-weight structure lines)
        wrong-item (first (filter #(unequal-col (:weights %))
                                  (map #(get structure %) lines)))]
    (calculate-value structure (:supporting wrong-item) (:weights wrong-item))))

(defn -main [& args]
  (println "part1: " (name (solve-1 (input))))
  (println "part2: " (solve-2 input)))
