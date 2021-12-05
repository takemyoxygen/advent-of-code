(ns advent-of-code-2016.day17
  (:require [advent-of-code-2016.common :as common])
  (:import (clojure.lang PersistentQueue)))

(def open-door-states #{\b \c \d \e \f})

(def steps [{:direction \U :offset [0 -1]}
            {:direction \D :offset [0 1]}
            {:direction \L :offset [-1 0]}
            {:direction \R :offset [1 0]}])

; up, down, left, right
(def move-offsets [[0 -1] [0 1] [-1 0] [1 0]])

(def open? (partial contains? open-door-states))

(defn move
  [from step]
  {:position (map + (:position from) (:offset step)) :path (str (:path from) (:direction step))})

(defn in-bounds?
  [{[x y] :position}]
  (and (>= 3 x 0) (>= 3 y 0)))

(defn next-doors
  [input path]
  (->> (str input path)
       (common/md5)
       (take 4)
       (map open?)))

(defn possible-moves
  [current-position input]
  (->> (next-doors input (:path current-position))
       (map vector steps)
       (filter second)
       (map first)
       (map #(move current-position %))
       (filter in-bounds?)))

(defn start-queue
  [start-coords]
  (conj PersistentQueue/EMPTY {:position start-coords :path ""}))

(defn next-queue
  [queue current-position input]
  (apply conj (pop queue) (possible-moves current-position input)))

(defn find-shortest-path
  [start destination input]
  (loop [queue (start-queue start)]
    (if-let [current-position (peek queue)]
      (if (= (:position current-position) destination)
          current-position
          (recur (next-queue queue current-position input))))))

(defn find-longest-path
  [start destination input]
  (loop [queue (start-queue start) longest nil?]
    (if-let [current-position (peek queue)]
      (if (= (:position current-position) destination)
        (recur (pop queue) (count (:path current-position)))
        (recur (next-queue queue current-position input) longest))
      longest)))

(defn solve-part-1
  [input]
  (->> input
       (find-shortest-path [0 0] [3 3])
       (:path)))

(defn solve-part-2
  [input]
  (find-longest-path [0 0] [3 3] input))