(ns adventofcode.day15)

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def mod-value    2147483647)
(def lower-bits   65535)
(def rounds-part1 40000000)
(def rounds-part2 5000000)

(defn stepper [^long mod-value ^long factor ^long current]
  (mod (* current factor) mod-value))

(def stepper-a (partial stepper mod-value 16807))
(def stepper-b (partial stepper mod-value 48271))

(defn make-generator-a [] (drop 1 (iterate stepper-a 512)))
(defn make-generator-b [] (drop 1 (iterate stepper-b 191)))

(defn compare-lower-bits [^long a ^long b]
  (= ^long (bit-and ^int lower-bits a)
     ^long (bit-and ^int lower-bits b)))

(defn solve-1 []
  (count
   (filter identity
           (take rounds-part1
                 (map compare-lower-bits
                      (make-generator-a)
                      (make-generator-b))))))

(defn solve-2 []
  (count
   (filter identity
           (take rounds-part2
                 (map compare-lower-bits
                      (filter (fn [^long v] (= 0 (mod v 4)))
                              (make-generator-a))
                      (filter (fn [^long v] (= 0 (mod v 8)))
                              (make-generator-b)))))))

(defn -main [& args]
  (let [part-1 (time (solve-1))
        part-2 (time (solve-2))]
    (println "part1:" part-1)
    (println "part2:" part-2)))
