(ns advent-of-code-2016.day15)

(defn parse-input
  [input]
  (->> input
       (clojure.string/split-lines)
       (map (partial re-matches #"Disc #(\d+) has (\d+) positions.*at position (\d+).*"))
       (map (fn [[_ disc pos-count start-pos]]
              {
               :disc (Integer/parseInt disc)
               :positions (Integer/parseInt pos-count)
               :start (Integer/parseInt start-pos)}))))

(defn position-at
  [pressed-offset disc]
  (rem (+ (:start disc) (:disc disc) pressed-offset) (:positions disc)))

(defn perfect-timing?
  [discs pressed-offset]
  (every? (comp zero? (partial position-at pressed-offset)) discs))

(defn find-min-offset
  [disks]
  (->> (range)
       (filter (partial perfect-timing? disks))
       (first)))

(defn solve-part-1
  [input]
  (find-min-offset (parse-input input)))

(defn solve-part-2
  [input]
  (let [discs (parse-input input)
        updated-discs (concat discs [{:disc (inc (count discs)) :positions 11 :start 0}])]
    (find-min-offset updated-discs)))