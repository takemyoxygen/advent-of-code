(ns aoc2022.day4
  (:require [clojure.string :as str]
    [clojure.set :as set]))


(defn ranges-intersect? [s1 e1 s2 e2]
  (or (and (>= s1 s2) (<= e1 e2))
    (and (>= s2 s1) (<= e2 e1))))


(defn ranges-overlap? [s1 e1 s2 e2]
  (or (>= e2 s1 s2)
    (>= e1 s2 s1)))


(defn part1 [input]
  (->>
    (slurp input)
    (str/split-lines)
    (map #(re-seq #"\d+" %))
    (map #(map (fn [d] (Integer/parseInt d)) %))
    (filter #(apply ranges-intersect? %))
    (count)))


(defn part2 [input]
  (->>
    (slurp input)
    (str/split-lines)
    (map #(re-seq #"\d+" %))
    (map #(map (fn [d] (Integer/parseInt d)) %))
    (filter #(apply ranges-overlap? %))
    (count)))


(println "Day 4")
(println "Part 1:" (part1 "./input/day4.txt"))
(println "Part 2:" (part2 "./input/day4.txt"))
