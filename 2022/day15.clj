(ns aoc2022.day15
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]
    [clojure.math :refer [signum]]))


(defn dist [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))


(defn cannot-beacon? [items pos]
  (some #(<= (dist (% :sensor) pos) (% :radius)) items))


(def items
  (->>
    (slurp "./input/day15.txt")
    (str/split-lines)
    (map #(re-seq #"-?\d+" %))
    (map (fn [line] (map #(Integer/parseInt %) line)))
    (map #(partition 2 %))
    (map (fn [[sensor beacon]] 
      {:sensor (vec sensor) 
       :beacon (vec beacon) 
       :radius (dist sensor beacon)}))))


(defn get-intervals [base items]
  (keep 
    (fn [{rad :radius, [x y] :sensor}] 
      (let [vertical-dist (abs (- y base))
            dx (- rad vertical-dist)]
        (if (> 0 dx) nil [(- x dx) (+ x dx)])))
    items))


(defn part1 [items]
  (let [base      2000000
        beacons   (set (map :beacon items))
        intervals (sort (get-intervals base items))
        at-base   (count (filter #(= base (% 1)) beacons))
        [points]    (reduce
                      (fn [[cnt prev-end] [start end]]
                        (cond
                          (>= prev-end end) [cnt prev-end]
                          (<  prev-end start) [(+ cnt (- end start) 1) end]
                          :else [(+ cnt (- end prev-end)) end]))
                      [0 Integer/MIN_VALUE]
                      intervals)]
    (- points at-base)))

