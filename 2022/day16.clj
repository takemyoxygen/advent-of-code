(ns aoc2022.day16
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]
    [clojure.math :refer [signum]]
    [clojure.data.priority-map :refer [priority-map]]
    [clojure.math.combinatorics :as combo]))


(defn parse-line [line]
  (let
    [[valve & tunnels] (re-seq #"[A-Z]{2}" line)
    flow (re-find #"\d+" line)]
    {:valve valve :tunnels tunnels :flow (Integer/parseInt flow)}))


(defn parse-input [file]
  (->> (slurp file)
    (str/split-lines)
    (map parse-line)
    (map (fn[v] [(v :valve) v]))
    (into {})))



(defn distances [valves start destinations]
  (loop [queue (conj (clojure.lang.PersistentQueue/EMPTY) [start 0]) results {} visited (set nil)]
    (if (or (empty? queue) (empty? destinations)) results
      (let [[valve dist] (peek queue)
            queue' (pop queue)
            was-here? (contains? visited valve)
            results' (if (and (not was-here?) (contains? destinations valve)) (assoc results valve dist) results)
            queue'' 
              (if was-here? queue' 
                (->> 
                  (get-in valves [valve :tunnels]) 
                  (remove visited) 
                  (map (fn [v] [v (inc dist)])) 
                  (apply conj queue')))]
        (recur queue'' results' (conj visited valve))))))


(defn find-max-pressure [valves closed time]
  (let [dists (into {} (map (fn [v] [v (distances valves v (disj closed v))]) (conj closed "AA")))]
    (loop [queue (list ["AA" 1 0 closed]) best 0]
    (if (empty? queue) best
      (let [[[valve minute pressure closed] & rest] queue]
        (cond
          (> minute time) (recur rest (max pressure best))
          (empty? closed) (recur rest (max pressure best))
          :else (let [pressure' (+ pressure (* (get-in valves [valve :flow]) (inc (- time minute))))
            closed' (disj closed valve)
            queue' (->> 
              (dists valve)
              (filter (fn [[v d]] (contains? closed v)))
              (map (fn [[v d]] [v (+ 1 minute d) pressure' closed']))
              (apply conj rest))]
            (recur queue' (max pressure' best)))))))))


(defn part1 [file]
  (let 
    [valves (parse-input file)
     closed 
     (->> valves
       (vals)
       (filter #(< 0 (% :flow)))
       (map :valve)
       (set))]
    (find-max-pressure valves closed 30)))


(defn part2 [file]
  (let [
    valves (parse-input file)
    closed 
    (->> valves
     (vals)
     (filter #(< 0 (% :flow)))
     (map :valve)
     (set))
    cnt (quot (count closed) 2)
    chunks (map #(set %) (combo/combinations closed cnt))]
    (->> chunks
      (map (fn [chunk1]
        (+ (find-max-pressure valves chunk1 26) (find-max-pressure valves (set/difference closed chunk1) 26))))
      (apply max))))


(println "Day 16")
(println "Part 1:" (part1 "./input/day16.txt"))
(println "Part 2:" (part2 "./input/day16.txt"))

