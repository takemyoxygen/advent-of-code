(ns aoc2022.day21
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]
    [clojure.math :refer [signum]]))


(def ops {
  "+" +
  "-" -
  "/" /
  "*" *})

(defn parse-input [input]
  (->> input
    (str/split-lines)
    (map (fn [line]
      (let [[mk shout]    (str/split line #": ")
            shout-val     (match (str/split shout #" ")
                            [num] {:val (Integer/parseInt num)}
                            [op1 op op2] {:op1 op1 :op op :op2 op2})]
        [mk shout-val])))
    (into {})))


(defn resolve [mk monkeys]
  (match (monkeys mk)
    {:val val} val
    {:op1 op1 :op op :op2 op2} ((ops op) (resolve op1 monkeys) (resolve op2 monkeys))
    :else (throw (Exception. (str "Not matched:" mk (monkeys mk))))))


(defn set-monkey [mk val monkeys]
  (loop [mk mk val val]
    (if (= mk "humn") val
      (let [{op :op op1 :op1 op2 :op2}  (monkeys mk)
            res1                        (resolve op1 monkeys)
            res2                        (resolve op2 monkeys)
            [undef-mk def-val]          (if (Double/isNaN res1) [op1 res2] [op2 res1])]
        (->>
          (match [op undef-mk]
            ["+" _]     (-' val def-val)
            ["*" _]     (/ val def-val) 
            ["/" op1]   (*' val def-val)
            ["/" op2]   (/ val def-val)
            ["-" op1]   (+' val def-val)
            ["-" op2]   (-' def-val val ))
          (recur undef-mk))))))


(defn part1 [input]
  (->> input
    (parse-input)
    (resolve "root")))


(defn part2 [input]
  (let [monkeys               (assoc (parse-input input) "humn" {:val Double/NaN})
        {mk1 :op1 mk2 :op2}   (monkeys "root")
        [res1 res2]           [(resolve mk1 monkeys) (resolve mk2 monkeys)]
        [undef val]           (if (Double/isNaN res1) [mk1 res2] [mk2 res1])]
    (set-monkey undef val monkeys)))



(def test-input "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32")

(def input (slurp "./input/day21.txt"))


(println "Day 21")
(println "Part 1:" (part1 input))
(println "Part 2:" (part2 input))