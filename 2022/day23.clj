(ns aoc2022.day23
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]))


(defn parse-input [file]
  (->> file
    (slurp)
    (str/split-lines)
    (map-indexed (fn [y row] (keep-indexed #(when (= %2 \#) [%1 y]) row)))
    (apply concat)
    (set)))


(def direction-tests [
    [[[-1 -1] [0 -1] [1 -1]]   [0 -1]] ; N
    [[[-1 1] [0 1] [1 1]]      [0 1]]  ; S
    [[[-1 -1] [-1 0] [-1 1]]   [-1 0]]   ; W
    [[[1 -1] [1 0] [1 1]]      [1 0]]    ; E
  ])

(def all-directions 
  (->> direction-tests
    (map #(get-in % [0]))
    (apply concat)
    (set)))


(defn direction-tests-from [index]
  (->> direction-tests
    (count)
    (range)
    (map #(rem (+ index %) (count direction-tests)))
    (map #(direction-tests %))))


(defn no-elves? [pos offsets elves]
  (->> offsets
    (filter #(contains? elves (mapv + pos %)))
    (seq)
    (nil?)))


(defn propose-next [pos elves dir-index]
  (if 
    (no-elves? pos all-directions elves)  pos
    (let        [dir-tests    (direction-tests-from dir-index)
                 step  (->> dir-tests
                              (keep (fn [[offsets step]] (when (no-elves? pos offsets elves) step)))
                              (first))]
      (if (nil? step) 
        pos
        (mapv + pos step)))))


(defn ground-in-rect [elves]
  (let [min-x   (->> elves (map #(% 0)) (apply min))
        min-y   (->> elves (map #(% 1)) (apply min))
        max-x   (->> elves (map #(% 0)) (apply max))
        max-y   (->> elves (map #(% 1)) (apply max))
        height  (inc (- max-y min-y))
        width   (inc (- max-x min-x))]
    (- (* height width) (count elves))))


(defn next-elves [elves iter]
  (->> elves
    (map (fn [pos] [pos (propose-next pos elves iter)]))
    (group-by #(% 1))
    (map (fn [[dest entries]] [dest (mapv #(% 0) entries)]))
    (map (fn [[dest sources]] (if (= 1 (count sources)) [dest] sources)))
    (apply concat)
    (set)))


(defn part1 [file]
  (loop [elves      (parse-input file)
         iter       0]
    (if (= iter 10) (ground-in-rect elves)
      (let [elves'  (next-elves elves iter)]
        (recur elves' (inc iter))))))


(defn part2 [file]
  (loop [elves      (parse-input file)
         iter       0]
    (let [elves'  (next-elves elves iter)]
      (if (= elves elves') (inc iter) (recur elves' (inc iter))))))


(println "Day 23")
(println "Part 1" (part1 "./input/day23.txt"))
(println "Part 2" (part2 "./input/day23.txt"))
