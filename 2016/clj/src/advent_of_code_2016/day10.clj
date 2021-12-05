(ns advent-of-code-2016.day10)

(def initial-state {:output {} :bot {}})

(defn parse-line
  [line]
  (if-let [[_ val bot] (re-matches #"value (\d+) goes to bot (\d+)" line)]
    {:value (Integer/parseInt val) :bot (Integer/parseInt bot)}
    (if-let [[_ bot low-type low high-type high] (re-matches #"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)" line)]
      {:bot (Integer/parseInt bot) :low-type (keyword low-type) :low-dest (Integer/parseInt low) :high-type (keyword high-type) :high-dest (Integer/parseInt high)}
      (throw (Exception. (str "Invalid input: " line))))))

(defn try-include
  [state value & path]
  (let [current (get-in state path #{})]
    (if (> 2 (count current))
      (assoc-in state path (conj current value) )
      state)))

(defn process-direct
  [state {value :value bot :bot}]
  (try-include state value :bot bot))

(defn process-comparative
  [state {bot :bot low-type :low-type low-dest :low-dest high-type :high-type high-dest :high-dest}]
  (let [values (get-in state [:bot bot])]
    (if (= 2 (count values))
      (let [low-value (apply min values)
            high-value (apply max values)]
        (-> state
            (try-include low-value low-type low-dest)
            (try-include high-value high-type high-dest)
            (assoc-in [:bot bot] #{})))
      state)))

(defn process-instruction
  [state instruction]
  (cond
    (:value instruction) (process-direct state instruction)
    (:low-type instruction) (process-comparative state instruction)
    :else state))

(defn find-bot
  [required-values state]
  (some (fn [[bot bot-values]] (and (= bot-values required-values) bot)) (:bot state)))

(defn require-outputs
  [required-outputs state]
  (let [available-outputs (:output state)
        required-output-values (map (fn [output-key] (get available-outputs output-key)) required-outputs)]
    (if (some nil? required-output-values) nil required-output-values)))

(defn repeat-instructions
  [state instructions stop-when]
  (let [cycled-instructions (cycle instructions)]
    (loop [current-state state [current-instruction & rest-instructions] cycled-instructions]
      (if-let [stop-result (stop-when current-state)]
        stop-result
        (let [new-state (process-instruction current-state current-instruction)]
          (recur new-state rest-instructions))))))

(defn parse-input
  [input]
  (->> input
       (clojure.string/split-lines)
       (map parse-line)))

(defn solve
  [raw-input stop-when]
  (let [instructions (parse-input raw-input)]
    (repeat-instructions initial-state instructions stop-when)))

(defn solve-part1
  [raw-input]
  (solve raw-input (partial find-bot #{17 61})))

(defn solve-part2
  [raw-input]
  (let [output-values (solve raw-input (partial require-outputs [0 1 2]))]
    (apply * (flatten (map seq output-values)))))