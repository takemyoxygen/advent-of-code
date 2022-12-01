(ns aoc2022.day1
	(:require [clojure.string :as str]))

(defn to-int [x] (Integer/parseInt x))

(defn read-group [s]
	(map to-int (str/split-lines s)))

(defn read-input [file]
	(map read-group (str/split (slurp file) #"\n\n")))


(defn sum-best [n, input]
	(->> input
		(map #(reduce + %))
		(sort #(compare %2 %1))
		(take n)
		(reduce +)))

(def input (read-input "./input/day1.txt"))

(sum-best 1 input)
(sum-best 3 input)
