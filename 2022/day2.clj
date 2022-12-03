(ns aoc2022.day2
	(:require [clojure.string :as str]))

(defn play [a b]
	(cond
		(= a b) 3
		(contains? #{"AB", "CA", "BC"} (str a b)) 6
		:else 0))

(def mapping {\X \A 
              \Y \B 
              \Z \C})


(defn part1 [file]
	(->> 
	(slurp file)
	(str/split-lines )
	(map #(str/split % #" "))
	(map (fn [line] (map #(get % 0) line)))
	(map (fn [line] 
		(let [[their, mine] line
			  mine' (mapping mine)
			  outcome (play their mine')
			  val (- (int mine') 64)]
		   (+ outcome val))))
	(reduce +)))

(defn my-shape [theirs outcome]
	(cond 
		(= outcome \Y) theirs
		(= outcome \Z) (if (= theirs \C) \A (char (+ 1 (int theirs))))
		:else (if (= theirs \A) \C (char (- (int theirs) 1)))))


(defn part2 [file]
	(->> 
	(slurp file)
	(str/split-lines )
	(map #(str/split % #" "))
	(map (fn [line] (map #(get % 0) line)))
	(map (fn [line] 
		(let [[their, outcome] line
			  mine (my-shape their outcome)
			  val (- (int mine) 64)
			  outcome (play their mine)]
		   (+ outcome val))))
	(reduce +)))


(println "Day 2")
(println "Part 1:" (part1 "./input/day2.txt"))
(println "Part 2:" (part2 "./input/day2.txt"))





