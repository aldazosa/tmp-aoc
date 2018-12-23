(ns aoc-2018.data
  (:require [clojure.java.io :as io]))

(def first-input
  (->> "first-input.txt"
       io/resource
       io/reader
       line-seq
       (map clojure.edn/read-string)))


(def box-ids
  (->> "second-input.txt"
       io/resource
       io/reader
       line-seq))


(def claims
  (->> "third-input.txt"
       io/resource
       io/reader
       line-seq))
