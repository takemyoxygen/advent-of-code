(ns advent-of-code-2016.day25
  (:require [advent-of-code-2016.monorail :as monorail]))


(defn output-ends-ok?
  [output]
  (= (last output) (rem (inc (count output)) 2)))

(defn stop?
  [limit {output :output}]
  (or
    (= (count output) limit)
    (and
      (not-empty output)
      (not (output-ends-ok? output)))))


(defn execute
  [input limit initial]
  (->> input
       (monorail/parse-instructions)
       (monorail/get-empty-state)
       (monorail/set-register {:name "a"} initial)
       (monorail/execute-instructions (partial stop? limit))
       (:output))
  )

(defn execute-and-print
  [input limit initial]
  (let [result (execute input limit initial)]
    (println initial "-" result)
    result))


;Slightly modified original input - use two "add" instructions instead of nested loop in the
; original list of instructions
(def processed-input "add d 2550\nadd d a\njnz 1 6\ninc d\ndec b\njnz b -2\ndec c\njnz c -5\ncpy d a\njnz 0 0\ncpy a b\ncpy 0 a\ncpy 2 c\njnz b 2\njnz 1 6\ndec b\ndec c\njnz c -4\ninc a\njnz 1 -7\ncpy 2 b\njnz c 2\njnz 1 4\ndec b\ndec c\njnz 1 -4\njnz 0 0\nout b\njnz a -19\njnz 1 -21")

(->> (range)
     (map inc)
     (map (fn [x] [x (execute-and-print processed-input 100 x)]))
     (filter (fn [[_ output]] (output-ends-ok? output)))
     (first)
     (first))