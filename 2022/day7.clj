(ns aoc2022.day7
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]))


(defn process-command [command path filesystem]
  (match (str/split (str/trim command) #" ")
    ["$" "cd" ".."] [(pop path) filesystem]
    ["$" "cd" dir]  [(conj path dir) filesystem]
    ["$" "ls"]      [path filesystem]
    ["dir" dir]     
    (let [key (rest (reverse (interleave (conj path dir) (repeat :children))))
      filesystem' (update-in filesystem key (fn [_] {:type :dir :children {}}))]
      [path filesystem'])
    [size file]
    (let [key (rest (reverse (interleave (conj path file) (repeat :children))))
      filesystem' (update-in filesystem key (fn [_] {:type :file :size (Integer/parseInt size)}))]
      [path filesystem'])
    :else (throw (Exception. (str "Unsupported command:" command)))))


(defn find-folders [info acc]
  (match (info :type)
    :file [(info :size) acc]
    :dir (let [[size acc'] (->> (info :children)
     (vals)
     (reduce (fn [state child-info]
      (let [[total acc] state
        [sz acc'] (find-folders child-info acc)]
        [(+ total sz) acc'])) [0 acc]))
    acc'' (if (>= 100000 size) (+ acc' size) acc')]
    [size acc''])))

(defn folder-sizes [info results]
  (match (info :type)
    :file [(info :size) results]
    :dir (let [[size results'] (->> (info :children)
     (vals)
     (reduce (fn [state child-info]
      (let [[total res] state
        [sz res'] (folder-sizes child-info res)]
        [(+ total sz) res'])) [0 results]))]
    [size (conj results' size)])))


(defn part1 [input]
  (let [[_ filesystem] 
    (->>
      (str/split-lines (slurp input))
      (reduce (fn [state command] (apply process-command command state)) [(list) {"/" {:type :dir :children {}}}]))
    [_ sizes] (folder-sizes (filesystem "/") (list))
    ]
    (->> sizes
      (filter #(>= 100000 %))
      (apply +))))

(defn part2 [input]
  (let [[_ filesystem] 
    (->>
      (str/split-lines (slurp input))
      (reduce (fn [state command] (apply process-command command state)) [(list) {"/" {:type :dir :children {}}}]))
    [total sizes] (folder-sizes (filesystem "/") (list))
    unused (- 70000000 total)
    to-free (- 30000000 unused)]
    (->> sizes
      (filter #(<= to-free %))
      (apply +))))


(println "Day 7:")
(println "Part 1:" (part1 "./input/day7.txt"))
(println "Part 2:" (part2 "./input/day7.txt"))
