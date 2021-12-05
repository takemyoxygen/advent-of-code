(ns advent-of-code-2016.day23
  (:require [advent-of-code-2016.monorail :as monorail]))

; several instructions in the original input are replaced with new "mult" instruction followed
; by setting a few registers to 0 and then jumping to the end of the multiplication (to keep the total number of instructions)
; the same as in the original input
(def input "cpy a b\ndec b\nmult a b\ncpy 0 c\ncpy 0 d\njnz 1 5\ndec c\njnz c -2\ndec d\njnz d -5\ndec b\ncpy b c\ncpy c d\ndec d\ninc c\njnz d -2\ntgl c\ncpy -16 c\njnz 1 c\ncpy 73 c\njnz 82 d\ninc a\ninc d\njnz d -2\ninc c\njnz c -5")

; Part 1
(->> input
     (monorail/parse-instructions)
     (monorail/get-empty-state)
     (monorail/set-register {:name "a"} 7)
     (monorail/execute-instructions)
     (:registers)
     (println "Part 1"))

; Part 2
(->> input
     (monorail/parse-instructions)
     (monorail/get-empty-state)
     (monorail/set-register {:name "a"} 12)
     (monorail/execute-instructions)
     (:registers)
     (println "Part 2"))
