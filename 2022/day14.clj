(ns aoc2022.day14
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]
    [clojure.math :refer [signum]]))


(defn apply-rocks [rocks path]
  (loop
    [[[x1 y1] [x2 y2] & rest] path
     rocks rocks]
    (if (nil? x2) rocks
      (->>
        (for
          [x (range (min x1 x2) (inc (max x1 x2)))
           y (range (min y1 y2) (inc (max y1 y2)))]
          [x y])
        (apply conj rocks)
        (recur (conj rest [x2 y2]))))))


(defn resting-location [[x y :as pos] blocked [min-x max-x max-y]]
  (loop
    [[x y :as pos] [500 0]]
    (cond
      ;abyss
      (or (<= max-x x) (>= min-x x) (>= y max-y))
      nil

      ; straight down
      (not (contains? blocked [x (inc y)]))
      (recur [x (inc y)])

      ; down left
      (not (contains? blocked [(dec x) (inc y)]))
      (recur [(dec x) (inc y)])

      ; down right
      (not (contains? blocked [(inc x) (inc y)]))
      (recur [(inc x) (inc y)])

      ; rest here
      :else pos)))


(defn part1 [rocks]
  (let
    [max-x (apply max (map #(% 0) rocks))
     min-x (apply min (map #(% 0) rocks))
     max-y (apply max (map #(% 1) rocks))
     boundaries [min-x max-x max-y]]
    (loop [cnt 0 blocked rocks]
      (let [resting (resting-location [500 0] blocked boundaries)]
        (if (nil? resting)
          cnt
          (recur (inc cnt) (conj blocked resting)))))))


(defn part2 [rocks]
  (let 
    [max-y (apply max (map #(% 1) rocks))
     start [500 0]
     boundaries [-500 1200 (+ 2 max-y)]
     rocks-with-floor (apply-rocks rocks [[-500 (+ 2 max-y)] [1200 (+ 2 max-y)]])]
    (loop [cnt 0 blocked rocks-with-floor]
      (if (contains? blocked start) 
        cnt
        (let [resting (resting-location start blocked boundaries)]
          (if (nil? resting) (throw (Exception. "Sand falls into the abyss!!!"))
            (recur (inc cnt) (conj blocked resting))))))))


(def rocks
    (->>
      (slurp "./input/day14.txt")
      (str/split-lines)
      (map (fn [line]
        (->> line
          (re-seq #"\d+")
          (map #(Integer/parseInt %))
          (partition 2))))
      (reduce apply-rocks (set nil))))


(println "Day 14")
(println "Part 1:" (part1 rocks))
(println "Part 2:" (part2 rocks))

