(ns aoc2022.day12
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]
    [clojure.math :refer [signum]]))

(defn elevation [p]
  (match p
    \S (int \a)
    \E (int \z)
    :else (int p)))

(defn can-go? [from to]
  (>= 1 (- (elevation to) (elevation from))))

(defn find [grid target]
  (->>
    grid
    (map-indexed 
      (fn 
        [y row]
        (let [x (.indexOf row target)]
          (if (= -1 x) nil [x y]))))
    (filter #(not (nil? %)))
    (first)))

(defn adjacent [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])


(defn part1 [grid]
  (loop
    [visited (set nil)
     queue (conj (clojure.lang.PersistentQueue/EMPTY) [(find grid \S) 0])]
    (let 
      [[[x y :as pos] dist] (peek queue)]
      (cond 
        (= ((grid y) x) \E) dist
        (contains? visited pos) (recur visited (pop queue))
        :else 
          (->> pos
            (adjacent)
            (filter #(not (contains? visited %)))
            (filter (fn [[x y]] (and (>= x 0) (>= y 0) (< y (count grid)) (< x (count (grid 0))))))
            (filter (fn [[ax ay]] (can-go? ((grid y) x) ((grid ay) ax))))
            (map (fn [pos] [pos (inc dist)]))
            (apply conj (pop queue))
            (recur (conj visited pos)))))))


(defn part2 [grid]
  (loop
    [visited (set nil)
     queue (conj (clojure.lang.PersistentQueue/EMPTY) [(find grid \E) 0])]
    (let 
      [[[x y :as pos] dist] (peek queue)]
      (cond 
        (or (= ((grid y) x) \S) (= ((grid y) x) \a)) dist
        (contains? visited pos) (recur visited (pop queue))
        :else 
          (->> pos
            (adjacent)
            (filter #(not (contains? visited %)))
            (filter (fn [[x y]] (and (>= x 0) (>= y 0) (< y (count grid)) (< x (count (grid 0))))))
            (filter (fn [[ax ay]] (can-go? ((grid ay) ax) ((grid y) x))))
            (map (fn [pos] [pos (inc dist)]))
            (apply conj (pop queue))
            (recur (conj visited pos)))))))

(def grid 
  (->>
    (slurp "./input/day12.txt")
    (str/split-lines)
    (mapv #(str/split % #""))
    (mapv (fn [line] (mapv #(get % 0) line)))))


(println "Day 12")
(println "Part 1:" (part1 grid))
(println "Part 2:" (part2 grid))

