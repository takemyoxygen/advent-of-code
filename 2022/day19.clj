(ns aoc2022.day19
  (:require [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.match :refer [match]]
    [clojure.pprint :refer [pprint]]
    [clojure.math :refer [signum]]))


(def resource-types [:ore :clay :obsidian :geode])


(defn enough-resources? [blueprint resources type]
  (every? 
    (fn [[res-type res-count]] (<= res-count (resources res-type))) 
    (blueprint type)))


(defn state-from [blueprint]
  {
    :blueprint      blueprint
    :minute         1
    :robots         {:ore 1 :clay 0 :obsidian 0 :geode 0}
    :resources      {:ore 0 :clay 0 :obsidian 0 :geode 0}
    :build-history  (list)
    })


(def max-req-robots 
  (memoize 
    (fn [blueprint] 
      (let [reqs    (map blueprint resource-types)]
        (->> resource-types
          (map (fn [type] [type (apply max (map #(get % type 0) reqs))]))
          (into {}))))))


(defn can-be-built [current prev-resources blueprint]
  (if (enough-resources? blueprint (current :resources) :geode)
    (list [{:geode 1} (blueprint :geode)]) ; always build geode if possible
    (->
        (keep 
          (fn [type]
            (if (and
                  (< (get-in current [:robots type]) ((max-req-robots blueprint) type))
                  (enough-resources? blueprint (current :resources) type)
                  (or 
                    (not (enough-resources? blueprint prev-resources type))
                    (not (nil? (first (current :build-history)))))
                  )
              [{type 1} (blueprint type)]
              nil))
          resource-types)
        (conj [{} {}]))))


(defn get-next-states [prev current]
  (let [collected     (current :robots)
        build-options (can-be-built 
                          current
                          (prev :resources) 
                          (current :blueprint))]
    (map 
      (fn [[built req-resources]]
        {
          :blueprint  (current :blueprint)
          :minute     (inc (current :minute))
          :robots     (merge-with + (current :robots) built)
          :resources  (merge-with -
                        (merge-with + (current :resources) collected)
                        req-resources)
          :build-history (conj (current :build-history) (first (keys built)))
        }) 
      build-options)))


(defn check-all-options [initial-state time]
  (loop [best     (get-in initial-state [:resources :geode])
         queue    (conj (clojure.lang.PersistentQueue/EMPTY) [initial-state initial-state])]
    (if (empty? queue) best
      (let [[prev current] (peek queue)]
        (let [next-states       (get-next-states prev current)
              best'             (->> next-states 
                                  (map #(get-in % [:resources :geode])) 
                                  (apply max best))]
          (if (= time (current :minute))
            (recur best' (pop queue))

            (->> next-states
              (map (fn [st] [current st]))
              (apply conj (pop queue))
              (recur best'))))))))


(defn get-next-state [state built-robot]
  (let [blueprint   (state :blueprint)
        built       (if (nil? built-robot) {} {built-robot 1})
        required    (if (nil? built-robot) {} (blueprint built-robot))]
    {
      :blueprint      blueprint
      :minute         (inc (state :minute))
      :robots         (merge-with + (state :robots) built)
      :resources      (merge-with -
                        (merge-with + (state :resources) (state :robots))
                        required)
      :build-history  (conj (state :build-history) built-robot)
  }))


(def test-blueprints
  [
    {
      :id        1
      :ore       {:ore 4}
      :clay      {:ore 2}
      :obsidian  {:ore 3 :clay 14}
      :geode     {:ore 2 :obsidian 7}
    }
    {
      :id        2
      :ore       {:ore 2}
      :clay      {:ore 3}
      :obsidian  {:ore 3 :clay 8}
      :geode     {:ore 3 :obsidian 12}
    }
])

(def blueprints [
  {:id 1 :ore {:ore 3} :clay {:ore 4} :obsidian {:ore 4 :clay 18} :geode {:ore 3 :obsidian 8 }}
  {:id 2 :ore {:ore 4} :clay {:ore 4} :obsidian {:ore 2 :clay 18} :geode {:ore 4 :obsidian 20}}
  {:id 3 :ore {:ore 3} :clay {:ore 4} :obsidian {:ore 4 :clay 18} :geode {:ore 4 :obsidian 12}}
  {:id 4 :ore {:ore 4} :clay {:ore 4} :obsidian {:ore 4 :clay 20} :geode {:ore 2 :obsidian 8 }}
  {:id 5 :ore {:ore 2} :clay {:ore 3} :obsidian {:ore 2 :clay 14} :geode {:ore 3 :obsidian 8 }}
  {:id 6 :ore {:ore 3} :clay {:ore 3} :obsidian {:ore 3 :clay 17} :geode {:ore 4 :obsidian 8 }}
  {:id 7 :ore {:ore 4} :clay {:ore 3} :obsidian {:ore 4 :clay 6} :geode {:ore 3 :obsidian 11}}
  {:id 8 :ore {:ore 2} :clay {:ore 4} :obsidian {:ore 4 :clay 20} :geode {:ore 4 :obsidian 18}}
  {:id 9 :ore {:ore 3} :clay {:ore 4} :obsidian {:ore 4 :clay 16} :geode {:ore 3 :obsidian 15}}
  {:id 10 :ore {:ore 4} :clay {:ore 4} :obsidian {:ore 3 :clay 7} :geode {:ore 4 :obsidian 11}}
  {:id 11 :ore {:ore 4} :clay {:ore 4} :obsidian {:ore 2 :clay 9} :geode {:ore 3 :obsidian 19}}
  {:id 12 :ore {:ore 4} :clay {:ore 3} :obsidian {:ore 2 :clay 17} :geode {:ore 3 :obsidian 16}}
  {:id 13 :ore {:ore 3} :clay {:ore 4} :obsidian {:ore 4 :clay 5} :geode {:ore 3 :obsidian 12}}
  {:id 14 :ore {:ore 3} :clay {:ore 3} :obsidian {:ore 3 :clay 8} :geode {:ore 2 :obsidian 12}}
  {:id 15 :ore {:ore 4} :clay {:ore 4} :obsidian {:ore 3 :clay 7} :geode {:ore 3 :obsidian 20}}
  {:id 16 :ore {:ore 3} :clay {:ore 3} :obsidian {:ore 2 :clay 11} :geode {:ore 2 :obsidian 19}}
  {:id 17 :ore {:ore 4} :clay {:ore 4} :obsidian {:ore 2 :clay 10} :geode {:ore 3 :obsidian 14}}
  {:id 18 :ore {:ore 3} :clay {:ore 4} :obsidian {:ore 3 :clay 12} :geode {:ore 3 :obsidian 17}}
  {:id 19 :ore {:ore 4} :clay {:ore 4} :obsidian {:ore 4 :clay 7} :geode {:ore 2 :obsidian 19}}
  {:id 20 :ore {:ore 2} :clay {:ore 3} :obsidian {:ore 2 :clay 17} :geode {:ore 3 :obsidian 19}}
  {:id 21 :ore {:ore 4} :clay {:ore 3} :obsidian {:ore 2 :clay 14} :geode {:ore 4 :obsidian 11}}
  {:id 22 :ore {:ore 4} :clay {:ore 3} :obsidian {:ore 4 :clay 15} :geode {:ore 4 :obsidian 9 }}
  {:id 23 :ore {:ore 4} :clay {:ore 4} :obsidian {:ore 3 :clay 9} :geode {:ore 3 :obsidian 7 }}
  {:id 24 :ore {:ore 4} :clay {:ore 4} :obsidian {:ore 4 :clay 15} :geode {:ore 4 :obsidian 17}}
  {:id 25 :ore {:ore 4} :clay {:ore 4} :obsidian {:ore 4 :clay 9} :geode {:ore 2 :obsidian 20}}
  {:id 26 :ore {:ore 3} :clay {:ore 4} :obsidian {:ore 3 :clay 20} :geode {:ore 3 :obsidian 14}}
  {:id 27 :ore {:ore 2} :clay {:ore 4} :obsidian {:ore 2 :clay 15} :geode {:ore 3 :obsidian 16}}
  {:id 28 :ore {:ore 2} :clay {:ore 3} :obsidian {:ore 3 :clay 8} :geode {:ore 3 :obsidian 20}}
  {:id 29 :ore {:ore 3} :clay {:ore 4} :obsidian {:ore 2 :clay 14} :geode {:ore 3 :obsidian 14}}
  {:id 30 :ore {:ore 4} :clay {:ore 3} :obsidian {:ore 4 :clay 18} :geode {:ore 3 :obsidian 13}}
])


(defn part1 [blueprints]
  (->> blueprints
  (map #(* (% :id) (check-all-options (state-from %) 24)))
  (apply +)))


(defn part2 [blueprints]
  (->> blueprints
    (take 3)
    (map #(check-all-options (state-from %) 32))
    (apply *)))


(println "Day 19")
(println "Part 1:" (part1 blueprints))
(println "Part 2:" (part2 blueprints))
