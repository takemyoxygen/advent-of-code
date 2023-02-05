(ns aoc2022.day24
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]))


(def directions {
  \>  [1 0]
  \<  [-1 0]
  \^  [0 -1]
  \v  [0 1]})

(def deltas [
    [0 0]
    [1 0]
    [-1 0]
    [0 1]
    [0 -1]
  ])

(defn denormalize-blizzards [blizz-directions]
  {:directions blizz-directions 
   :locations (->> blizz-directions (map first) (set))})


(defn parse-input [file]
  (let [lines     (->> file
                    (slurp)
                    (str/split-lines))
        height    (count lines)
        width     (count (lines 0))
        start-x   (str/index-of (lines 0) \.)
        dest-x    (str/index-of (lines (dec height)) \.)
        blizzards (for [x       (range width) 
                        y       (range height)
                        :let    [char (get (lines y) x)]
                        :when   (contains? directions char)]
                    [[x y] (directions char)])]
    {:width         width 
     :height        height 
     :start         [start-x 0] 
     :destination   [dest-x (dec height)]
     :blizzards     (denormalize-blizzards blizzards)}))


(defn adjacent-to [pos {height :height width :width start :start dest :destination}]
  (->> deltas
    (map #(mapv + pos %))
    (filter (fn [[x y :as adj]] 
      (or
        (= start adj)
        (= dest adj)
        (and (< 0 x (dec width)) (< 0 y (dec height))))))))


(defn advance-blizzards [blizzards {height :height width :width}]
  (->> (blizzards :directions)
    (mapv 
      (fn [[pos dir]]
        (let  [[x y]      (mapv + pos dir)
               next-pos   (cond
                            (= x 0)             [(- width 2) y]
                            (= x (dec width))   [1 y]
                            (= y 0)             [x (- height 2)]
                            (= y (dec height))  [x 1]
                            :else               [x y])]
          [next-pos dir])))
    (denormalize-blizzards)))


(def advance-blizzards-to
  (memoize (fn [time config]
              (if (zero? time) 
                (config :blizzards)
                (advance-blizzards
                  (advance-blizzards-to (dec time) config)
                  config)))))


(defn covered-by-blizzard? [pos {locations :locations}]
  (contains? locations pos))


(defn find-time [start start-time destination config]
  (loop [visited  (set nil)
         queue    (conj (clojure.lang.PersistentQueue/EMPTY) [start start-time])]
    (let [[pos time :as current]  (peek queue)
          queue'                  (pop queue)]
      (cond 
        (empty? queue) (throw (Exception. "Destination unreachable"))
        (= pos destination) time
        (contains? visited current) (recur visited queue')
        :else
        (let [time'       (inc time)
              visited'    (conj visited current)
              blizzards   (advance-blizzards-to time' config)
              adj         (adjacent-to pos config)]
          (->> adj
            (remove #(covered-by-blizzard? % blizzards))
            (map (fn [p] [p time']))
            (remove #(contains? visited %))
            (apply conj queue')
            (recur visited')))))))


(defn part1 [file]
  (let [{start :start dest :destination :as config} (parse-input file)]
    (find-time start 0 dest config)))



(defn part2 [file]
  (let [{start :start dest :destination :as config} (parse-input file)
        time1   (find-time start 0 dest config)
        time2   (find-time dest time1 start config)
        time3   (find-time start time2 dest config)]
    time3))


(println "Day24")
(println "Part 1" (part1 "./input/day24.txt"))
(println "Part 2" (part2 "./input/day24.txt"))