(ns aoc2022.day9
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]
    [clojure.math :refer [signum]]))


(def directions {
  "U" [0 1] 
  "D" [0 -1]
  "R" [1 0]
  "L" [-1 0]})


(defn parse-input [file]
  (->>
    (slurp file)
    (str/split-lines)
    (map (fn [line] 
      (let [[dir steps] (str/split line #" ")]
        [dir (Integer/parseInt steps)])))))


(defn move-along [head tail]
  (let [diffs (map - head tail)
        move-tail? (some #(< 1 (abs %)) diffs)]
    (if move-tail? (mapv + tail (map #(int (signum %)) diffs)) tail)))


(defn part1 [input]
  (->>
    (parse-input input)
    (mapcat (fn [[dir steps]] (repeat steps dir)))
    (reduce
      (fn [[visited head tail] dir]
        (let [head' (mapv + head (directions dir))
              tail' (move-along head' tail)]
          [(conj visited tail') head' tail']))
      [(set [[0 0]]) [0 0] [0 0]])
    (first)
    (count)))


(defn part2 [input]
  (->>
    (parse-input input)
    (mapcat (fn [[dir steps]] (repeat steps dir)))
    (reduce
      (fn [[visited rope] dir]
        (let [rope' 
                (reduce
                  (fn 
                    [rope-part knot]
                    (->> (if 
                            (empty? rope-part) 
                            (mapv + knot (directions dir)) 
                            (move-along (first rope-part) knot))
                          (conj rope-part))) 
                  (list) 
                  rope)
                tail (first rope')]
          [(conj visited tail) (reverse rope')]))
      [(set [[0 0]]) (repeat 10 [0 0])])
    (first)
    (count)))


(println "Day 9")
(println "Part 1:" (part1 "./input/day9.txt"))
(println "Part 2:" (part2 "./input/day9.txt"))
