(ns aoc2022.day5
  (:require [clojure.string :as str]
    [clojure.set :as set]))


(defn parse-instructions [instructions]
  (->> instructions
    (str/split-lines)
    (map #(re-seq #"\d+" %))
    (map (fn [tokens] (map #(Integer/parseInt %) tokens)))))

(defn parse-stacks [stacks]
  (let [lines (str/split-lines stacks)
        [stack-lines, [last-line]] (split-at (dec (count lines)) lines)
        n (->> last-line
            (re-seq #"\d+")
            (last)
            (Integer/parseInt))
        stacks (repeatedly n #(list))]
    (reduce 
      (fn [stacks line] 
        (map-indexed 
          (fn [idx stack] 
            (let [ch-idx (inc (* 4 idx))
                  ch (get line ch-idx)]
                  (if (Character/isUpperCase ch)
                      (conj stack ch)
                      stack))) 
          stacks)) 
      stacks 
      (reverse stack-lines))))


(defn apply-instructions [stacks instructions reverse?]
  (reduce 
    (fn [stacks instruction] 
      (let [[count src dst] instruction
            [to-move src'] (split-at count (get stacks (dec src)))
            to-move-ordered (if reverse? (reverse to-move) to-move)
            dst' (apply conj (get stacks (dec dst)) to-move-ordered)]
            (-> (vec stacks)
              (assoc (dec src) src')
              (assoc (dec dst) dst')))) 
    (vec stacks) 
    instructions))


(defn part1 [input]
  (let [[stacks-lines instructions-lines] (str/split (slurp input) #"\n\n")
      stacks (vec (parse-stacks stacks-lines))
      instructions (parse-instructions instructions-lines)]
    (->> (apply-instructions stacks instructions false)
        (map first)
        (apply str))))


(defn part2 [input]
  (let [[stacks-lines instructions-lines] (str/split (slurp input) #"\n\n")
      stacks (vec (parse-stacks stacks-lines))
      instructions (parse-instructions instructions-lines)]
    (->> (apply-instructions stacks instructions true)
        (map first)
        (apply str))))


(println "Day 5:")
(println "Part 1" (part1 "./input/day5.txt"))
(println "Part 2" (part2 "./input/day5.txt"))




