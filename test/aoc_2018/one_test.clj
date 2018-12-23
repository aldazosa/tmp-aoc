(ns aoc-2018.one-test
  (:require [aoc-2018.one :as sut :refer :all]
            [clojure.test :as t :refer :all])
  (:refer-clojure :exclude [reductions]))


(def test-frequencies [+1 -2 +3 +1])


(deftest part-one
  (testing "Part one"
    (is (= 3 (frequency test-frequencies)))
    (is (= 3 (frequency [+1 +1 +1])))
    (is (= 0 (frequency [+1 +1 -2])))
    (is (= -6 (frequency [-1 -2 -3])))))


(deftest part-two
  (testing "Part two"
    (is (= 2
           (first-duplicate-frequency test-frequencies)))
    (is (zero?
          (first-duplicate-frequency [+1 -1])))
    (is (= 10
           (first-duplicate-frequency [+3 +3 +4 -2 -4])))
    (is (= 5
           (first-duplicate-frequency [-6 +3 +8 +5 -6])))
    (is (= 14
           (first-duplicate-frequency [+7 +7 -2 -7 -4])))))
