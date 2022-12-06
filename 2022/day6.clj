(ns aoc2022.day6
  (:require [clojure.string :as str]
    [clojure.set :as set]))

(defn solve [input window-size]
  (loop [start 0
         end 0]
    (cond
      (<= (count input) end) nil
      :else
      (if 
        (->>
          (range start end)
          (map #(get input %))
          (some #(= (get input end) %)))

        (recur (inc start) end)
        (if (= (dec window-size) (- end start)) (inc end) (recur start (inc end))))
      )))


(defn part1 [input]
  (solve input 4))


(defn part2 [input]
  (solve input 14)


; bvwbjplbgvbhsrlpgdmjqwftvncz: first marker after character 5
; nppdvjthqldpwncqszvftbrmjlhg: first marker after character 6
; nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg: first marker after character 10
; zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw: first marker after character 11
(part1 "bvwbjplbgvbhsrlpgdmjqwftvncz")
(part1 "nppdvjthqldpwncqszvftbrmjlhg")
(part1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
(part1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")


(println "Day 6:")
(println "Part 1" (part1 (slurp "./input/day6.txt")))
(println "Part 2" (part2 "./input/day6.txt"))




