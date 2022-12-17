(ns aoc2022.day11
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]
    [clojure.math :refer [signum]]))

(defn queue
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
    (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))


(def test-monkeys [
  {:items (queue [79 98])
   :op #(* 19 %)
   :test [23 2 3]
   :inspected 0}

 {:items (queue [54 65 75 74])
   :op #(+ 6 %)
   :test [19 2 0]
   :inspected 0}

  {:items (queue [79 60 97])
   :op #(* %1 %1)
   :test [13 1 3]
   :inspected 0}

 {:items (queue [74])
   :op #(+ 3 %1)
   :test [17 0 1]
   :inspected 0}
])


(def monkeys [
  {:items (queue [66, 71, 94])
   :op #(* 5 %1)
   :test [3 7 4]
   :inspected 0}

  {:items (queue [70])
   :op #(+ 6 %1)
   :test [17 3 0]
   :inspected 0}

  {:items (queue [62, 68, 56, 65, 94, 78])
   :op #(+ 5 %1)
   :test [2 3 1]
   :inspected 0}


  {:items (queue [89, 94, 94, 67])
   :op #(+ 2 %1)
   :test [19 7 0]
   :inspected 0}

  {:items (queue [71, 61, 73, 65, 98, 98, 63])
   :op #(* 7 %1)
   :test [11 5 6]
   :inspected 0}


  {:items (queue [55, 62, 68, 61, 60])
   :op #(+ 7 %1)
   :test [5 2 1]
   :inspected 0}

  {:items (queue [93, 91, 69, 64, 72, 89, 50, 71])
   :op #(+ 1 %1)
   :test [13 5 2]
   :inspected 0}

  {:items (queue [76, 50])
   :op #(* %1 %1)
   :test [7 4 6]
   :inspected 0}
  ])


(defn round [monkeys next-state]
  (loop [monkey-index 0
         monkeys monkeys]
     (if (= monkey-index (count monkeys))
        monkeys
          (let [monkey (monkeys monkey-index)
                items (monkey :items)
                monkeys'
                  (reduce
                      (fn [monkeys worry]
                        (let [
                          [next-monkey-idx worry'] (next-state monkey worry)]
                          (update-in monkeys [next-monkey-idx :items] #(conj % worry'))))
                      monkeys
                      items)
                monkeys''
                  (->
                    monkeys'
                    (assoc-in [monkey-index :items] (queue nil))
                    (update-in [monkey-index :inspected] #(+ % (count items))))]
            (recur (inc monkey-index) monkeys'')))))


(defn next-state-part1 [monkey worry]
  (let [worry' (quot ((monkey :op) worry) 3)
        [val t f] (monkey :test)   
        next-monkey-idx (if (= 0 (mod worry' val)) t f)]
    [next-monkey-idx worry']))


(defn part1 [monkeys]
  (->> 
    (reduce (fn [mk _] (round mk next-state-part1)) monkeys (range 20))
    (map #(% :inspected))
    (sort)
    (reverse)
    (take 2)
    (apply *)))


(defn next-state-part2 [monkey worry]
  (let [worry' (into {} 
          (map (fn [[base rem]] [base (mod ((monkey :op) rem) base)]) worry))
        [val t f] (monkey :test)   
        next-monkey-idx (if (= 0 (worry' val)) t f)]
    [next-monkey-idx worry']))


(defn part2 [monkeys]
  (let [tests (map #(first (% :test)) monkeys)
        monkeys' 
          (mapv 
            (fn [mk]
              (update mk :items 
                (fn [items] 
                  (mapv 
                    (fn [item] (into {} (map (fn [tst] [tst (mod item tst)]) tests))) 
                    items)))) 
            monkeys)]
    (->> 
      (reduce (fn [mk _] (round mk next-state-part2)) monkeys' (range 10000))
      (map #(% :inspected))
      (sort)
      (reverse)
      (take 2)
      (apply *))))

(println "Day 11")
(println "Part 1:" (part1 monkeys))
(println "Part 2:" (part2 monkeys))

