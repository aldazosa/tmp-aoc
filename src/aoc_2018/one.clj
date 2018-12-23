(ns aoc-2018.one
  (:require [clojure.spec.alpha :as s]
            [aoc-2018.data :as data])
  (:refer-clojure :exclude [reductions]))


;;;;;;;;;;;;;;;;
;; First part ;;
;;;;;;;;;;;;;;;;

(defn frequency
  [frequencies]
  (reduce + frequencies))


;;;;;;;;;;;;;;;;;
;; Second part ;;
;;;;;;;;;;;;;;;;;

(defn stop-at-duplicate
  [coll]
  (reduce (fn [acc x]
            (if (contains? acc x)
              (reduced x)
              (conj acc x)))
          #{}
          coll))


(defn first-duplicate-frequency
  [coll]
  (->> coll
       cycle
       (cons 0)
       (reductions +)
       stop-at-duplicate))
