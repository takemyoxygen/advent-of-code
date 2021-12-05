(ns advent-of-code-2016.day14
  (:require [advent-of-code-2016.common :as common]))

(defn contains-repetitive-char
  [counts? target-count input]
  (loop [current-symbol (get input 0) next-index 1 repetition-count 1]
    (let [next-symbol (get input next-index)]
      (cond
        (= repetition-count target-count) current-symbol
        (nil? next-symbol) nil
        (counts? current-symbol next-symbol) (recur current-symbol (inc next-index) (inc repetition-count))
        :else (recur next-symbol (inc next-index) 1)))))

(def find-triplet (partial contains-repetitive-char = 3))

(defn contains-5-in-row
  [symbol input]
  ((complement nil?) (contains-repetitive-char (partial = symbol) 5 input)))

(defn md5-stretch [s]
  (reduce (fn [input _] (common/md5 input)) s (range 2017)))

(defn hashes
  [input hashing-fn]
  (map (fn [x] [x (hashing-fn (str input x))]) (range)))

(defn key?
  [hash next-hashes]
  (if-let [repeated (find-triplet hash)]
    (some (fn [[idx hash]] (contains-5-in-row repeated hash)) (take 1000 next-hashes))
    false))

(defn find-keys
  [salt keys-required hashing-fn]
  (loop [[[index hash] & rest] (hashes salt hashing-fn) found-keys 0]
    (cond
      (= keys-required found-keys) (dec index)
      (key? hash rest) (recur rest (inc found-keys))
      :else (recur rest found-keys))))

(defn solve-part-1
  [salt]
  (find-keys salt 64 common/md5))

(defn solve-part-2
  [salt]
  (find-keys salt 64 md5-stretch))