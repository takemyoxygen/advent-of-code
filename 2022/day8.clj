(ns aoc2022.day8
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]))


(defn remove-smaller [items x]
  (loop [xs items]
    (if (or (empty? xs) (>= (first (first xs)) x)) xs
      (recur (rest xs)))))


(defn find-visible-trees [outers inners get-val]
  (reduce 
    (fn 
      [visibles outer]
      (first 
        (reduce 
          (fn 
            [[visibles stack] inner]
            (let [val (get-val outer inner)
              stack' (remove-smaller stack val)
              visible? (empty? stack')
              visibles' (if visible? (conj visibles (list outer inner)) visibles)
              stack'' (conj stack' [val inner])]
              [visibles' stack'']))
          [visibles (list)] 
          inners
          )))
      (set nil) 
      outers))


(defn calculate-viewing-distances [outers inners get-val map-coord]
  (reduce 
    (fn 
      [distances outer]
      (first 
        (reduce 
          (fn 
            [[distances stack] inner]
            (let [val (get-val outer inner)
                  stack' (remove-smaller stack val)
                  last-visible (if (empty? stack') (first inners) ((first stack') 1))
                  distance (abs (- last-visible inner))
                  distances' (assoc distances (map-coord (list outer inner)) distance)
                  stack'' (conj stack' [val inner])]
              [distances' stack'']))
          [distances (list)] 
          inners
          )))
      {} 
      outers))

(defn parse-grid [file]
  (->> 
    (slurp file)
    (str/split-lines)
    (map #(str/split % #""))
    (map #(map (fn [ch] (Integer/parseInt ch)) %))
    (map vec)
    (vec)))


(defn part1 [input]
  (let [grid (parse-grid input)
        ys (range (count grid))
        xs (range (count (grid 0)))
        top-down (find-visible-trees xs ys #((grid %2) %1))
        bottom-up (find-visible-trees xs (reverse ys) #((grid %2) %1))
        left-right (set (map reverse (find-visible-trees ys xs #((grid %1) %2))))
        right-left (set (map reverse (find-visible-trees ys (reverse xs) #((grid %1) %2))))]
    (count (set/union top-down bottom-up right-left left-right))))


(defn part2 [input]
  (let [grid (parse-grid input)
        ys (range (count grid))
        xs (range (count (grid 0)))
        top-down (calculate-viewing-distances xs ys #((grid %2) %1) identity)
        bottom-up (calculate-viewing-distances xs (reverse ys) #((grid %2) %1) identity)
        left-right (calculate-viewing-distances ys xs #((grid %1) %2) reverse)
        right-left (calculate-viewing-distances ys (reverse xs) #((grid %1) %2) reverse)
        coords (keys top-down)
        maps [top-down bottom-up left-right right-left]]
    (->> coords
      (map (fn [coord] (reduce #(* %1 (%2 coord)) 1 maps)))
      (apply max))))



(println "Day 8:")
(println "Part 1:" (part1 "./input/day8.txt"))
(println "Part 2:" (part2 "./input/day8.txt"))
