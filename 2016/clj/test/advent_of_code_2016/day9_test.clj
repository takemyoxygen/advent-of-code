(ns advent-of-code-2016.day9-test
  (:require [clojure.test :refer :all]
            advent-of-code-2016.day9))

(deftest part1
  (testing "no markers"
    (is (= (advent-of-code-2016.day9/solve-part1 "ADVENT") (count "ADVENT"))))

  (testing "single marker"
    (is (= (advent-of-code-2016.day9/solve-part1 "A(1x5)BC") (count "ABBBBBC"))))

  (testing "single marker at start"
    (is (= (advent-of-code-2016.day9/solve-part1 "(3x3)XYZ") (count "XYZXYZXYZ"))))

  (testing "several markers"
    (is (= (advent-of-code-2016.day9/solve-part1 "A(2x2)BCD(2x2)EFG") (count "ABCBCDEFEFG"))))

  (testing "markers in repeated part"
    (is (= (advent-of-code-2016.day9/solve-part1 "X(8x2)(3x3)ABCY") (count "X(3x3)ABC(3x3)ABCY"))))

  )

(deftest part2
  (testing "no markers"
    (is (= (advent-of-code-2016.day9/solve-part2 "ADVENT") (count "ADVENT"))))

  (testing "single marker"
    (is (= (advent-of-code-2016.day9/solve-part2 "(3x3)XYZ") (count "XYZXYZXYZ"))))

  (testing "one nested marker"
    (is (= (advent-of-code-2016.day9/solve-part2 "X(8x2)(3x3)ABCY") (count "XABCABCABCABCABCABCY"))))

  (testing "multiple nested markers"
    (is (= (advent-of-code-2016.day9/solve-part2 "(27x12)(20x12)(13x14)(7x10)(1x12)A") 241920)))

  (testing "multiple nested markers #2"
    (is (= (advent-of-code-2016.day9/solve-part2 "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") 445)))

  )