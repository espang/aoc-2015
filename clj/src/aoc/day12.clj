(ns day12
  (:require
   [clojure.data.json :as json]))

(def input
  (-> "./src/aoc/input_12.txt"
      slurp
      json/read-str))

(defn map-has-red-property [m]
  (seq (filter #(= "red" %) (vals m))))

(defmulti count-nums class)
(defmethod count-nums clojure.lang.PersistentHashMap [m]
  (if (map-has-red-property m)
    0
    (reduce (fn [acc [_ v]] (+ acc (count-nums v)))
            0
            m)))
(defmethod count-nums clojure.lang.PersistentArrayMap [m]
  (if (map-has-red-property m)
    0 
    (reduce (fn [acc [_ v]] (+ acc (count-nums v)))
              0
              m)))
(defmethod count-nums clojure.lang.PersistentVector [v]
  (reduce (fn [acc v]
            (+ acc (count-nums v)))
          0
          v))
(defmethod count-nums java.lang.Long [l] l)
(defmethod count-nums java.lang.String [s] 0)

(count-nums input)