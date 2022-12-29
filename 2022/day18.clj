(ns aoc2022.day18
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]
    [clojure.math :refer [signum]]))

(def steps [
  [1 0 0]
  [-1 0 0]
  [0 1 0]
  [0 -1 0]
  [0 0 1]
  [0 0 -1]])

(defn get-adjacent [pos [min-x min-y min-z] [max-x max-y max-z]]
  (->> steps
    (map #(mapv + pos %))
    (filter (fn [[x y z]]
      (and
        (<= min-x x max-x)
        (<= min-y y max-y)
        (<= min-z z max-z))))))

(defn parse-input [input]
  (->> input
    (re-seq #"\d+")
    (map #(Integer/parseInt %))
    (partition 3)
    (map vec)
    (set)))


(defn part1 [input]
  (let [cubes (parse-input input)]
  (->> cubes
    (map (fn [[x y z]] 
      (+
        (if (contains? cubes [(inc x) y z]) 1 0)
        (if (contains? cubes [x (inc y) z]) 1 0)
        (if (contains? cubes [x y (inc z)]) 1 0))))
    (apply +)
    (* 2)
    (- (* 6 (count cubes))))))


(defn get-bounding-box [cubes]
  (let [mins (->> (range 3)
                (map (fn [x] (apply min (map #(% x) cubes))))
                (mapv dec))
        maxs (->> (range 3)
                (map (fn [x] (apply max (map #(% x) cubes))))
                (mapv inc))]
    [mins maxs]))


(defn part2 [input]
  (let [lava-cubes (parse-input input)
        [box-min box-max] (get-bounding-box lava-cubes)]
    (loop [visited (set nil) 
           border 0
           queue (conj (clojure.lang.PersistentQueue/EMPTY) box-min)]
      (if (empty? queue) border
        (let [pos (peek queue)
              adj (get-adjacent pos box-min box-max)
              adj-lavas (count (filter lava-cubes adj))]
            (if (contains? visited pos) (recur visited border (pop queue))
              (->> adj
                (remove lava-cubes)
                (remove visited)
                (apply conj (pop queue))
                (recur (conj visited pos) (+ border adj-lavas)))))))))


(def test-input "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5")

(def input (slurp "./input/day18.txt"))


(println "Day 18")
(println "Part 1:" (part1 input))
(println "Part 2:" (part2 input))


