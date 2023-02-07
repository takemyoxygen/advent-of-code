(ns aoc2022.day25
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.math :refer [pow]]
    [clojure.pprint :refer [pprint]]))


(def digits-map {
  \2 2
  \1 1
  \0 0
  \- -1
  \= -2
  })


(defn normalize [snafu]
  (->> snafu
    (reverse)
    (map digits-map)
    (apply list)))


(defn pad [snafu target-size]
  (apply conj (repeat (- target-size (count snafu)) 0) (reverse snafu)))


(defn add [s1 s2]
  (let [s1            (pad s1 (max (count s1) (count s2)))
        s2            (pad s2 (count s1))
        pairs         (map vector s1 s2)
        [res carry]   (reduce
                        (fn [[res carry] [d1 d2]]
                          (let [val           (+ carry d1 d2)
                                [val carry']  (cond
                                                (> -2 val)  [(+ val 5) -1]
                                                (< 2 val)   [(- val 5) 1]
                                                :else       [val 0])]
                            [(conj res val) carry']))
                        [(list) 0]
                        pairs)]
    (reverse (if (zero? carry) res (conj res carry)))))


(defn to-string [normalized]
  (let [lookup (set/map-invert digits-map)]
    (->> normalized
      (map lookup)
      (reverse)
      (apply str))))


(defn part1 [file]
  (->> "./input/day25.txt"
    (slurp) 
    (str/split-lines)
    (map normalize)
    (reduce add (list 0))
    (to-string)))


(println "Day 25")
(println "Part 1" (part1 "./input/day25.txt"))

