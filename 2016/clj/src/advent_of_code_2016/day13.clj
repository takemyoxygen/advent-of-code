(ns advent-of-code-2016.day13
  (:import (clojure.lang PersistentQueue)))

(defn one-bits-count
  [input]
  (loop [x input ones 0]
    (if (= 0 x)
      ones
      (let [bit (rem x 2)
            x-next (quot x 2)
            ones-next (if (= bit 1) (inc ones) ones)]
        (recur x-next ones-next)))))

(defn wall?
  [x y input]
  (let [magic-number (+ (* x x) (* 3 x) (* 2 x y) y (* y y) input)
        ones-count (one-bits-count magic-number)]
    (= 1 (rem ones-count 2))))

(defn adjacent-coords
  [[x y]]
  [[(inc x) y] [x (inc y)] [(dec x) y] [x (dec y)]])

(defn traverse
  [start input stop?]
  (loop [queue (conj PersistentQueue/EMPTY [start 0]) reached-points #{}]
    (let [[current-point current-steps] (peek queue)]
      (cond
        (nil? current-point) reached-points
        (contains? reached-points current-point) (recur (pop queue) reached-points)
        :else (let [moves (->> current-point
                         (adjacent-coords)
                         (filter (fn [[x y]] (and (>= x 0) (>= y 0))))
                         (filter (fn [[x y]] (not (wall? x y input))))
                         (filter (fn [point] (not (contains? reached-points point))))
                         (map (fn [point] [point (inc current-steps)])))]
          (if (stop? current-point current-steps)
            [current-steps reached-points]
            (recur (apply conj (pop queue) moves) (conj reached-points current-point))))))))

(defn solve-part-1
  [input]
  (first (traverse [1 1] input (fn [current-point _] (= current-point [31 39])))))


(defn solve-part-2
  [input]
  (->> (traverse [1 1] input (fn [_ current-steps] (> current-steps 50)))
       (second)
       (count)))