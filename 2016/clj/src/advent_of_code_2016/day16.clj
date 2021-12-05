(ns advent-of-code-2016.day16)

(defn generate-next
  [input]
  (let [addition (map {\1 \0 \0 \1} (reverse input))]
    (concat input (cons \0 addition) )))

(defn generate-data
  [target-length initial-input]
  (->> initial-input
       (iterate generate-next)
       (filter #(>= (count %) target-length))
       (first)
       (take target-length)))

(defn next-checksum
  [input]
  (->> input
       (partition 2)
       (map (fn [[a b]] (if (= a b) \1 \0)))))

(defn generate-checksum
  [data]
  (->> data
       (iterate next-checksum)
       (filter (comp odd? count))
       (first)))

(defn solve
  [size input]
  (->> input
       (generate-data size)
       (generate-checksum)
       (apply str)))

(def solve-part-1 (partial solve 272))

(def solve-part-2 (partial solve 35651584))