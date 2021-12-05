(ns advent-of-code-2016.day21)

(defn to-int [x] (Integer/parseInt x))

(defn split-words [input] (clojure.string/split input #"\s+"))

;swap position X with position Y
(def swap-position-op
  {:op :swap-position
   :extract-args
       (fn [[swap position x _ _ y]]
         (if (and (= swap "swap") (= position "position")) [(to-int x) (to-int y)]))
   :execute
      (fn [input x y]
        (let [input-vec (vec input)]
          (map-indexed
            (fn [idx it]
              (cond
                (= x idx) (input-vec y)
                (= y idx) (input-vec x)
                :else it))
            input)))})

;swap letter X with letter Y
(def swap-letter-op
  {:op :swap-letter
   :extract-args
      (fn [[swap letter x _ _ y]]
        (if (and (= swap "swap") (= letter "letter")) [(first x) (first y)]))
   :execute
      (fn [input x y]
        (map
          (fn [i] (cond
                    (= i x) y
                    (= i y) x
                    :else i))
          input))})

;rotate left/right X steps
(def rotate-dir-op
  {:op :rotate-dir-op
   :extract-args
      (fn [[rotate direction x]]
        (if (and (= rotate "rotate") (or (= direction "left") (= direction "right")))
            [(keyword direction) (to-int x)]))
   :execute
      (fn [input direction steps]
        (let [normalized-steps (rem steps (count input))
              pivot-point (case direction :left normalized-steps :right (- (count input) normalized-steps))
              [left right] (split-at pivot-point input)]
          (concat right left)))})

;rotate based on position of letter X
(def rotate-letter-op
  {:op :rotate-letter-op
   :extract-args
      (fn [[rotate based _ _ _ _ x]]
        (if (and (= rotate "rotate") (= based "based"))
            [(first x)]))
   :execute
      (fn [input x]
        (let [input-vec (vec input)
              index (.indexOf input-vec x)
              steps (+ (inc index) (if (>= index 4) 1 0))]
          ((:execute rotate-dir-op) input :right steps)))})

(defn rotate-letter-rev
  [input x]
  (let [input-vec (vec input)
        index (.indexOf input-vec x)
        prev-index (if (= (rem index 2) 1)
                       (quot (dec index) 2)
                       (quot (+ (count input-vec) index -2) 2))
        steps (mod (- prev-index index) (count input-vec))]
    (println "index" index)
    (println "prev-index" prev-index)
    (println "steps" steps)
    ((:execute rotate-dir-op) input :right steps)))

;reverse positions X through Y
(def reverse-pos-op
  {:op :reverse-pos-op
   :extract-args
      (fn [[reverse _ x _ y]]
        (if (= reverse "reverse") [(to-int x) (to-int y)]))
   :execute
      (fn [input x y]
        (let [[first interim] (split-at x input)
              [middle last] (split-at (inc (- y x)) interim)]
          (concat first (reverse middle) last)))})

;move position X to position Y
(def move-position-op
  {:op :move-position-op
   :extract-args
      (fn [[move _ x _ _ y]]
        (if (= move "move")
          [(to-int x) (to-int y)]))
   :execute
      (fn [input x y]
        (let [input-vec (vec input)
              target (input-vec x)]
          (reverse (loop [[x & rest :as current-input] input acc [] idx 0]
                     (cond
                       (= idx (count input-vec)) acc
                       (= y idx) (recur current-input (cons target acc) (inc idx))
                       (= x target) (recur rest acc idx)
                       :else (recur rest (cons x acc) (inc idx)))))))})

(defn create-reverse-op
  ([op] (create-reverse-op op identity (:execute op)))
  ([op rev-args] (create-reverse-op op rev-args (:execute op)))
  ([op rev-args rev-exec]
    (assoc op
      :op (keyword (str (name (:op op)) "-rev"))
      :extract-args (fn [op-def]
                      (some->> op-def
                               ((:extract-args op))
                               (rev-args)))
      :execute rev-exec))
  )

(def all-ops
  [swap-position-op
   swap-letter-op
   rotate-dir-op
   rotate-letter-op
   reverse-pos-op
   move-position-op])

(def all-rev-ops
  [(create-reverse-op swap-position-op)
   (create-reverse-op swap-letter-op)
   (create-reverse-op rotate-dir-op (fn [[direction x]]
                                      (if (= direction :left) [:right x] [:left x])))
   (create-reverse-op rotate-letter-op identity rotate-letter-rev)
   (create-reverse-op reverse-pos-op)
   (create-reverse-op move-position-op reverse)])

(defn find-op-def
  [ops op]
  (->> ops
       (map (fn [op-def] [((:extract-args op-def) op) op-def]))
       (filter first)
       (first)))

(defn execute-op
  [ops input op]
  (if-let [[args op-def] (find-op-def ops op)]
    (let [result (apply (:execute op-def) input args)]
      (println "executing op" (:op op-def) "on input" (apply str input) "on args" args "result:" (apply str result))
      result)))

(defn parse-input
  [input]
  (->> input
       (clojure.string/split-lines)
       (map split-words)))

(defn solve-part-1
  [raw-op-defs initial]
  (->> raw-op-defs
       (parse-input)
       (reduce (partial execute-op all-ops) initial)
       (apply str)))


(defn solve-part-2
  [raw-op-defs initial]
  (->> raw-op-defs
       (parse-input)
       (reverse)
       (reduce (partial execute-op all-rev-ops) initial)
       (apply str)))

(def input "rotate right 3 steps\nswap position 7 with position 0\nrotate left 3 steps\nreverse positions 2 through 5\nmove position 6 to position 3\nreverse positions 0 through 4\nswap position 4 with position 2\nrotate based on position of letter d\nrotate right 0 steps\nmove position 7 to position 5\nswap position 4 with position 5\nswap position 3 with position 5\nmove position 5 to position 3\nswap letter e with letter f\nswap position 6 with position 3\nswap letter a with letter e\nreverse positions 0 through 1\nreverse positions 0 through 4\nswap letter c with letter e\nreverse positions 1 through 7\nrotate right 1 step\nreverse positions 6 through 7\nmove position 7 to position 1\nmove position 4 to position 0\nmove position 4 to position 6\nmove position 6 to position 3\nswap position 1 with position 6\nswap position 5 with position 7\nswap position 2 with position 5\nswap position 6 with position 5\nswap position 2 with position 4\nreverse positions 2 through 6\nreverse positions 3 through 5\nmove position 3 to position 5\nreverse positions 1 through 5\nrotate left 1 step\nmove position 4 to position 5\nswap letter c with letter b\nswap position 2 with position 1\nreverse positions 3 through 4\nswap position 3 with position 4\nreverse positions 5 through 7\nswap letter b with letter d\nreverse positions 3 through 4\nswap letter c with letter h\nrotate based on position of letter b\nrotate based on position of letter e\nrotate right 3 steps\nrotate right 7 steps\nrotate left 2 steps\nmove position 6 to position 1\nreverse positions 1 through 3\nrotate based on position of letter b\nreverse positions 0 through 4\nswap letter g with letter c\nmove position 1 to position 5\nrotate right 4 steps\nrotate left 2 steps\nmove position 7 to position 2\nrotate based on position of letter c\nmove position 6 to position 1\nswap letter f with letter g\nrotate right 6 steps\nswap position 6 with position 2\nreverse positions 2 through 6\nswap position 3 with position 1\nrotate based on position of letter h\nreverse positions 2 through 5\nmove position 1 to position 3\nrotate right 1 step\nrotate right 7 steps\nmove position 6 to position 3\nrotate based on position of letter h\nswap letter d with letter h\nrotate left 0 steps\nmove position 1 to position 2\nswap letter a with letter g\nswap letter a with letter g\nswap position 4 with position 2\nrotate right 1 step\nrotate based on position of letter b\nswap position 7 with position 1\nrotate based on position of letter e\nmove position 1 to position 4\nmove position 6 to position 3\nrotate left 3 steps\nswap letter f with letter g\nswap position 3 with position 1\nswap position 4 with position 3\nswap letter f with letter c\nrotate left 3 steps\nrotate left 0 steps\nrotate right 3 steps\nswap letter d with letter e\nswap position 2 with position 7\nmove position 3 to position 6\nswap position 7 with position 1\nswap position 3 with position 6\nrotate left 5 steps\nswap position 2 with position 6")

(solve-part-1 input "abcdefgh")

(solve-part-2 input "fbgdceah")
