(ns adventofcode.day16
  (:require [clojure.java.io :as io]
            [clojure.set :as cs]
            [clojure.string :as str]))

;; (set! *warn-on-reflection* true)
;; (set! *unchecked-math* :warn-on-boxed)

(def input (-> "day16/input" io/resource slurp))

(defn start-positions ^chars [] (char-array 16 "abcdefghijklmnop"))

;;         0  1   2      3   4       5      6   7       8
(def regex #"(s([0-9]{1,2}))|(x([0-9]{1,2})/([0-9]{1,2}))|(p([a-p])/([a-p]))")

(defn spin ^chars [^chars dancers ^long amount]
  (let [[head tail] (split-at (- 16 amount) dancers)]
    (char-array 16 (concat tail head))))

(defn exchange! ^chars [^chars dancers ^long a ^long b]
  (let [old-a (aget dancers a)
        old-b (aget dancers b)]
    (aset dancers a old-b)
    (aset dancers b old-a)
    dancers))

(defn partner! ^chars [^chars dancers a b]
  (let [dancers-vec (into [] dancers)
        position-a  (.indexOf dancers-vec a)
        position-b  (.indexOf dancers-vec b)]
    (exchange! dancers position-a position-b)))

(defn handle-move ^chars [^chars dancers parsed-move]
  (let [[move a b] parsed-move]
    (condp = move
      :spin (spin dancers a)
      :exchange (exchange! dancers a b)
      :partner (partner! dancers a b)
      dancers)))

(defn parse-move [regex-vector]
  (let [[match
         spin-match spin-number
         exchange-match exchange-a exchange-b
         partner-match partner-a partner-b]
        regex-vector]
    (cond
      spin-match [:spin (read-string spin-number)]
      exchange-match [:exchange (read-string exchange-a) (read-string exchange-b)]
      partner-match [:partner (.charAt ^String partner-a 0) (.charAt ^String partner-b 0)]
      :else [])))

(defn dance [parsed-moves positions]
  (apply str
         (into []
               (reduce handle-move
                       (char-array 16 positions)
                       parsed-moves))))

(defn solve-1 [input]
  (let [matches (re-seq regex input)]
    (dance (map parse-move matches) (start-positions))))

(defn solve-2 [input]
  (let [matches      (re-seq regex input)
        parsed-moves (mapv parse-move matches)]
    (:positions
     (reduce (fn [{:keys [positions lookup seen-at]} i]
               (let [lookup-value (get lookup positions)]
                 (if lookup-value
                   (if (= 0
                          (mod (- 1000000000 ^int i)
                               (- ^int i ^int (get seen-at positions))))
                     (reduced {:positions positions})
                     {:positions lookup-value
                      :lookup lookup
                      :seen-at (assoc seen-at positions i)})
                   (let [new-positions (dance parsed-moves positions)]
                     {:positions new-positions
                      :lookup    (assoc lookup positions new-positions)
                      :seen-at   (assoc seen-at positions i)}))))
             {:positions (start-positions)
              :lookup    {}
              :seen-at   {}}
             (range 1000000000)))))

(defn -main [& args]
  (time (println (solve-1 input)))
  (time (println (solve-2 input))))
