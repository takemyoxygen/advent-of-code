(ns aoc2022.day3
	(:require [clojure.string :as str]
		[clojure.set :as set]))


(defn priority [type] 
	(if (Character/isLowerCase type)
		(- (int type) 96)
		(- (int type) 38)))


(defn part1 [file]
	(->>
		(slurp file)
		(str/split-lines)
		(map (fn [line] 
			(let [[left right] (split-at (/ (count line) 2) line)]
				(set/intersection (set left) (set right)))))
		(mapcat (fn [common]
			(map priority common)))
		(apply +)))


(defn part2 [file]
	(->>
		(slurp file)
		(str/split-lines)
		(partition 3)
		(mapcat #(apply set/intersection (map set %)))
		(map priority)
		(apply +)))


(println "Day 3")
(println "Part 1:" (part1 "./input/day3.txt"))
(println "Part 2:" (part2 "./input/day3.txt"))
