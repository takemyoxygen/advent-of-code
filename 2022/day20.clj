(ns aoc2022.day20
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]
    [clojure.math :refer [signum]]))


(defn index-of [pred vec] ;vec - transient vector
  (loop [i 0]
    (cond
      (>= i (count vec)) (throw (Exception. "Unable to find!"))
      (pred (vec i)) i
      :else (recur (inc i)))))


(defn calc-target-pos [idx-from xs]
  (let [el          (xs idx-from)
        val-base    (rem (el :val) (dec (count xs)))
        to-base     (+ idx-from val-base)]
    (cond
      (>= to-base (count xs)) (inc (mod to-base (count xs)))
      (< to-base 0) (dec (mod to-base (count xs)))
      :else to-base)))


(defn move [idx-from xs]
  (let [idx-to    (calc-target-pos idx-from xs)
        dir       (int (signum (- idx-to idx-from)))
        d-idx     (* -1 dir)
        idxs      (range (+ idx-from dir) (+ idx-to dir) dir)
        el        (xs idx-from)]
    (->
      (reduce
        (fn [current idx] (assoc! current (+ idx d-idx) (current idx)))
        xs
        idxs)
      (assoc! idx-to el))))


(defn keyify [xs]
  (->
    (map-indexed (fn [idx x] {:key idx :val x}) xs)
    (vec)))


(defn mix [input]
  (reduce
    (fn [xs key] (move (index-of #(= (% :key) key) xs) xs))
    input
    (range (count input))))


(defn get-coord [mixed]
  (let [base-idx  (index-of #(= 0 %) mixed)]
    (->> [1000 2000 3000]
      (map (fn [idx] (mixed (mod (+ idx base-idx) (count mixed)))))
      (apply +))))


(defn part1 [input]
  (->> input
    (keyify)
    (transient)
    (mix)
    (persistent!)
    (mapv :val)
    (get-coord)
  ))


(defn part2 [input]
  (let [decription-key  811589153
        input'          (mapv #(* decription-key %) input)
        mix10           (apply comp (repeat 10 mix))]
  (->> input'
    (keyify)
    (transient)
    (mix10)
    (persistent!)
    (mapv :val)
    (get-coord))))


(def input 
  (->> (slurp "./input/day20.txt")
    (str/split-lines)
    (mapv #(Integer/parseInt %))))


(def test-input [1, 2, -3, 3, -2, 0, 4])

(println "Day 20")
(println "Part 1:" (part1 input))
(println "Part 2:" (part2 input))

