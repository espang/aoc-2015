(ns day12
  (:require
   [clojure.data.json :as json]))


(def input
  (-> "./src/aoc/input_12.txt"
      slurp
      json/read-str))

(defn count-numbers [[k v]]
  (println "key: " k "|| value: " v))

(map count-numbers input)

(defmulti foo class)
(defmethod foo ::collection [c] :a-collection)
(defmethod foo String [s] :a-string)

(defmulti count-nums class)
(defmethod count-nums clojure.lang.PersistentHashMap [m]
  (reduce (fn [acc [_ v]] (+ acc (count-nums v)))
          0
          m))

(defmethod count-nums clojure.lang.PersistentArrayMap [m]
  (reduce (fn [acc [_ v]] (+ acc (count-nums v)))
          0
          m))
(defmethod count-nums clojure.lang.PersistentVector [v]
  (reduce (fn [acc v]
            (+ acc (count-nums v)))
          0
          v))
(defmethod count-nums java.lang.Long [l] l)
(defmethod count-nums java.lang.String [s] 0)

(count-nums input)