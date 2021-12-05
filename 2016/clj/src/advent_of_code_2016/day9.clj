(ns advent-of-code-2016.day9)

(defn read-n
  "Reads first n symbols from given collection. Returns a pair - first n elements and the remaining ones"
  [n coll]
  [(take n coll) (drop n coll)])

(defn read-until
  "Reads symbols from incoming string until it encounters a given stop-at symbol. Returns
  a pair - symbols read before stop-at symbol and remaining string (starting with stop-at symbol)"
  [input stop-at]
  (loop [[x & rest :as current-input] input acc '()]
    (if
      (or (empty? current-input) (= x stop-at)) [(apply str (reverse acc)) current-input]
      (recur rest (cons x acc)))))

(defn read-repeat
  "Expects a string starting from repetition marker e.g. \"(3x10)ABC\", extracts repetition params
  and returns the rest of the input string e.g. [3 10 \"ABC\"]"
  [[_ & rest]]
  (let [[repeat-length after-repeat-count] (read-until rest \x)
        [repeat-times after-repeat-times] (read-until (next after-repeat-count) \))]
    [(Integer/parseInt repeat-length) (Integer/parseInt repeat-times) (next after-repeat-times)]))

(defn calculate-simple-repeated-length
  "Calculates the length of string which follows repetition marker without processing inner markers"
  [repeat-length repeat-times _]
  (* repeat-times repeat-length))

(declare calculate-decompressed-size)

(defn calculate-recursive-repeated-length
  [_ repeat-times to-repeat]
  (* repeat-times (calculate-decompressed-size to-repeat calculate-recursive-repeated-length)))

(defn process-repeat
  [input repetition-calc]
  (let [[repeat-length repeat-times rest] (read-repeat input)
        [to-repeat rest-after-repetition] (read-n repeat-length rest)
        repetition-length (repetition-calc repeat-length repeat-times to-repeat)]
    [repetition-length rest-after-repetition]))

(defn sanitize
  "Prepares input (removes whitespaces) to be passed further to calculation functions"
  [input]
  (-> input
      (clojure.string/replace #"\s+" "")
      (char-array)))

(defn calculate-decompressed-size
  "Decompresses repeatitions encountered in the given input and returns decompressed input length"
  [input repetition-calc]
  (loop [current-length 0 [c & rest :as to-process] input]
    (cond
      (nil? c) current-length
      (= c \() (let [[repeated-length rest-after-repetition] (process-repeat to-process repetition-calc)]
                 (recur (+ current-length repeated-length) rest-after-repetition))
      :else (recur (inc current-length) rest))))


(defn solve
  [input repetition-calc]
  (let [sanitized-input (sanitize input)]
    (calculate-decompressed-size sanitized-input repetition-calc)))

(defn solve-part1
  [input]
  (solve input calculate-simple-repeated-length))

(defn solve-part2
  [input]
  (solve input calculate-recursive-repeated-length))
