(ns advent-of-code-2016.day19)

(defn round
  [input]
  (loop [[x y & rest] input acc nil]
    (cond
      (nil? y) (next (reverse (cons x acc)))
      (nil? rest) (reverse (cons x acc))
      :else (recur rest (cons x acc)))))

(defn solve-part-1
  [input]
  (->> (range 1 (inc input))
       (iterate round)
       (drop-while #(> (count %) 1))
       (first)))

(defn all-elves
  [n]
  (vec (range 1 (inc n))))

(defn step
  [all i]
  ;(println all i)
  (let [steal-from (rem (+ i (quot (count all) 2)) (count all))]
    ;(println (str "Elf " (all i) " steals from elf " (all steal-from)))
    (filterv #(not= % (all steal-from)) all)))

(defn simulate
  [n]
  (loop [elves (all-elves n) index 0]
    (if (= 1 (count elves))
        elves
        (let [next-elves (step elves index)
              next-index (if (> (count next-elves) (inc index)) (inc index) 0)]
          (recur next-elves next-index)))))

(defn step-2
  [all]
  (let [total (count all)
        indices (range total)
        steal-from-indices (map (fn [i] (+ i i (quot (- total i) 2))) indices)
        valid-steal-from-indices (take-while #(> total %) steal-from-indices)
        elves-to-skip (apply hash-set (map all valid-steal-from-indices))
        partition-index (count valid-steal-from-indices)
        [processed not-processed] (split-at partition-index all)
        filtered-not-processed (filter #(not (contains? elves-to-skip %)) not-processed)
        next-all (concat filtered-not-processed processed)]
    ;(println (map all valid-steal-from-indices))
    (vec next-all)))

(defn simulate-2
  [n]
  (->> (all-elves n)
       (iterate step-2)
       (drop-while #(> (count %) 1))
       (first)))