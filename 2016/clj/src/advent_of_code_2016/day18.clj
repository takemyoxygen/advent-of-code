(ns advent-of-code-2016.day18)

(def trap? (partial = \^))
(def not-trap? (complement trap?))

(defn next-tile-trap?
  [[left center right]]
  (or
    (and (trap? left) (trap? center) (not-trap? right))
    (and (trap? center) (trap? right) (not-trap? left))
    (and (trap? left) (not-trap? right) (not-trap? center))
    (and (trap? right) (not-trap? left) (not-trap? center))))

(defn next-row
  [prev-row]
  (let [tiles (partition 3 1 (cons \. (concat prev-row [\.])))]
    (map (fn [t] (if (next-tile-trap? t) \^ \.)) tiles)))

(defn safe-count
  [row-count input]
  (->> (seq input)
       (iterate next-row)
       (take row-count)
       (pmap #(count (filter not-trap? %)))
       (reduce +)))

(def solve-part-1 (partial safe-count 40))
(def solve-part-2 (partial safe-count 400000))
