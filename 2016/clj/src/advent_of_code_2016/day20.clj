(ns advent-of-code-2016.day20)

(defn parse-input
  [input]
  (->> input
       (clojure.string/split-lines)
       (map #(re-matches #"(\d+)-(\d+)" %))
       (map (fn [[_ lo hi]] [(bigint lo) (bigint hi)]))))

(def max-ip 4294967295)

(defn open-ranges-seq
  [last-hi [[lo hi] & rest-blacklist]]
  (cond
    (nil? lo) (if (= last-hi max-ip) [] [[(inc last-hi) max-ip]])
    (> (- lo last-hi) 1) (cons [(inc last-hi) (dec lo)] (lazy-seq (open-ranges-seq (max hi last-hi) rest-blacklist)))
    :else (lazy-seq (open-ranges-seq (max hi last-hi) rest-blacklist))))

(defn get-open-ranges
  [input]
  (->> input
       (parse-input)
       (sort-by first)
       (open-ranges-seq 0)))

(def solve-part-1 (comp first first get-open-ranges))

(defn size [[lo hi]] (inc (- hi lo)))

(defn solve-part-2
  [input]
  (->> input
       (get-open-ranges)
       (map size)
       (reduce +)))
