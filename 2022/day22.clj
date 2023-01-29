(ns aoc2022.day22
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]
    [clojure.math :refer [signum]]))


(def directions [[1 0] [0 1] [-1 0] [0 -1]]) ;clock-wise order, starting from ->

(def direction-labels {
  [1 0]   :right
  [0 1]   :down
  [-1 0]  :left
  [0 -1]  :up
})


(defn key-by-val [map value]
  (->> map
      (keep #(when (= value (val %)) (key %)))
      (first)))


(defn turn [dir turn-instruction]
  (let [curr-idx  (.indexOf directions dir)
        d-idx     (if (= "R" turn-instruction) 1 -1)]
    (directions (mod (+ curr-idx d-idx) (count directions)))))


(defn tile-at [grid [x y]]
  (-> grid
    (get y [])
    (get x " ")))


(defn next-tile-flat [grid position direction _]
  (let [next-pos      (mapv + position direction)
        tile-at-next  (tile-at grid next-pos)]
    (match tile-at-next
      (:or "." "#") [next-pos direction]
      " " (loop [opp-dir  (turn (turn direction "L") "L")
                 pos      position]
            (let [pos'  (mapv + pos opp-dir)]
              (if (= (tile-at grid pos') " ") [pos direction]
                (recur opp-dir pos'))))
      :else (throw (Exception. (str "Unsupported tile at" next-pos "-" tile-at-next))))))


(defn parse-input [file]
  (let [lines         (str/split-lines (slurp file))
        grid          (->> lines
                        (take-while #(not (empty? %)))
                        (mapv #(str/split % #"")))
        instructions  (->> lines
                        (drop-while #(not (empty? %)))
                        (drop 1)
                        (first)
                        (re-seq #"R|L|\d+")
                        (map (fn [x] (if (or (= x "L") (= x "R")) x (Integer/parseInt x)))))]
    [grid instructions]))


(defn password [[x y] direction]
  (+ (* 1000 (inc y) ) (* 4 (inc x)) (.indexOf directions direction)))



(defn tile-base [pos {size :size tiles :tiles}]
  (mapv #(* (quot % size) size) pos))


(defn coord-to-tile [pos {tiles :tiles :as config}]
  (let [base        (tile-base pos config)]
    (key-by-val tiles base)))


(defn get-offset [[x y :as pos] direction {size :size :as config}]
  (let [[base-x base-y] (tile-base pos config)]
    (case (direction-labels direction)
      :up     (- x base-x)
      :down   (dec (- size (- x base-x)))
      :right  (- y base-y)
      :left   (dec (- size (- y base-y))))))


(defn apply-offset [tile dir-label offset {size :size tiles :tiles}]
  (let [[base-x base-y]       (tiles tile)
        direction             (key-by-val direction-labels dir-label)
        point                 (case dir-label
                                :up     [(+ base-x offset) (+ base-y size -1)]
                                :down   [(+ base-x size (* -1 offset) -1) base-y]
                                :right  [base-x (+ base-y offset)]
                                :left   [(+ base-x size -1) (+ base-y size (* -1 offset) -1)])]
    [point direction]))


(defn next-tile-cubic [grid position direction {rules :rules :as config}]
  (let [next-pos      (mapv + position direction)
        tile-at-next  (tile-at grid next-pos)]
    (match tile-at-next
      (:or "." "#") [next-pos direction]
      " " (let [tile                (coord-to-tile position config)
                dir-label           (direction-labels direction)
                offset              (get-offset position direction config)
                [tile' dir-label']  (rules [tile dir-label])]
            (apply-offset tile' dir-label' offset config))
      :else (throw (Exception. (str "Unsupported tile at" next-pos "-" tile-at-next))))))



(def test-config {
  :size 4
  :file "./input/day22-test.txt"
  :tiles {
    1 [8 0]
    2 [0 4]
    3 [4 4]
    4 [8 4]
    5 [8 8]
    6 [12 8]
  }
  :rules{
    [1 :up]         [2 :down]
    [1 :right]      [6 :left]
    [1 :left]      [3 :down]
    [2 :up]         [1 :down]
    [2 :left]       [6 :up]
    [2 :down]       [5 :up]
    [3 :up]         [1 :right]
    [3 :down]       [5 :right]
    [4 :right]      [6 :down]
    [5 :left]       [3 :up]
    [5 :down]       [2 :up]
    [6 :up]         [4 :left]
    [6 :right]      [1 :left]
    [6 :down]       [2 :right]
  }
  })

(def config {
  :size 50
  :file "./input/day22.txt"
  :tiles {
    1 [50 0]
    2 [100 0]
    3 [50 50]
    4 [0 100]
    5 [50 100]
    6 [0 150]
  }
  :rules{
    [1 :up]       [6 :right]
    [1 :left]     [4 :right]
    [2 :up]       [6 :up]
    [2 :right]    [5 :left]
    [2 :down]     [3 :left]
    [3 :right]    [2 :up]
    [3 :left]     [4 :down]
    [4 :up]       [3 :right]
    [4 :left]     [1 :right]
    [5 :right]    [2 :left]
    [5 :down]     [6 :left]
    [6 :right]    [5 :up]
    [6 :down]     [2 :down]
    [6 :left]     [1 :down]
  }
  })


(defn solve [config get-next-state]
  (let [[grid instructions]   (parse-input (config :file))
        start-pos             [(.indexOf (grid 0) ".") 0]]
    (loop [position       start-pos
           direction      (directions 0)
           instructions   instructions]
      (match [instructions]
        [(_ :guard empty?)] 
        (password position direction)
        
        [([(instr :guard string?) & rest] :seq)] 
        (recur position (turn direction instr) rest)
        
        [([instr & rest] :seq)]
        (let [[pos' dir']   (get-next-state grid position direction config)
              remain        (dec instr)]
          (case (tile-at grid pos')
            "#" (recur position direction rest)
            "." (recur pos' dir' (if (zero? remain) rest (conj rest remain)))))))))



(defn part1 [config]
  (solve config next-tile-flat))


(defn part2 [config]
  (solve config next-tile-cubic))


(println "Day 22")
(println "Part 1:" (part1 config))
(println "Part 2:" (part2 config))
