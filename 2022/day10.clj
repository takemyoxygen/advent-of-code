(ns aoc2022.day10
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]
    [clojure.math :refer [signum]]))


(defn parse-input [file]
  (->> 
    (slurp file)
    (str/split-lines)
    (map (fn [line]
      (let [tokens (str/split line #" ")]
        (if (= 2 (count tokens)) [(tokens 0) (Integer/parseInt (tokens 1))] tokens))))
    (vec)))


(defn cycle-values [instructions]
  (->>
    (loop [val 1
         cycle 1
         acc (list)
         instructions instructions]
    (match instructions
      [] (conj acc val)
      [["noop"] & rest] 
        (recur val (inc cycle) (conj acc val) rest)
      [["addx" add] & rest] 
        (recur (+ val add) (+ 2 cycle) (conj (conj acc val) val) rest)))
    (reverse)
    (vec)))


(defn part1 [file]
  (let [instructions (parse-input file)
        cycles (cycle-values instructions)]
    (loop [pos 19 acc 0]
      (if 
        (>= pos (count cycles)) 
        acc 
        (recur (+ 40 pos) (+ acc (* (inc pos) (cycles pos))))))))


(defn part2 [file]
  (let [instructions (parse-input file)
        cycles (cycle-values instructions)
        screen (range 240)]
    (->>
      (map 
        (fn [px val]
          (if (>= 1 (abs (- (mod px 40) val))) \# \.))
        screen
        cycles)
      (partition 40)
      (map #(apply str %))
      (str/join "\n")
      (println))))


(println "Day 10")
(println "Part 1:" (part1 "./input/day10.txt"))
(println "Part 2:" (part2 "./input/day10.txt"))