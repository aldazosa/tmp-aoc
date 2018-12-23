(ns aoc-2018.two-test
  (:require [aoc-2018.two :as sut :refer :all]
            [clojure.test :as t :refer :all]))



(def test-ids ["abcdef"
               "bababc"
               "abbcde"
               "abcccd"
               "aabcdd"
               "abcdee"
               "ababab"])


(def second-part-ids ["abcde"
                      "fghij"
                      "klmno"
                      "pqrst"
                      "fguij"
                      "axcye"
                      "wvxyz"])


  (map #(if (= % %2) 0 1) x y)

(let [x (seq "abcde")
      y (seq "fghij")]
  (reduce))

(deftest part-one
  (testing "Part one"
    (is (= 12 (checksum test-ids)))
    (is (= 12 (transducer-checksum test-ids)))))


(deftest part-two
  (testing "Part two"
    ))
