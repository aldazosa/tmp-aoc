(ns aoc-2018.two
  (:require [clojure.spec.alpha :as s]
            [aoc-2018.data :as data]))


(defn checksum
  [ids]
  (->> ids
       (map frequencies)
       (map vals)
       (map (juxt #(if (some #{2} %) 1 0)
                  #(if (some #{3} %) 1 0)))
       (reduce #(map + % %2))
       (apply *)))



(def xf (comp
          (map frequencies)
          (map vals)
          (map (juxt #(if (some #{2} %) 1 0)
                     #(if (some #{3} %) 1 0)))))


(defn transducer-checksum
  [ids]
  (->> ids
     (into [] xf)
     (reduce #(map + % %2))
     (apply *)))


(defn delete [idx s]
  (str (subs s 0 idx) (subs s (inc idx))))


(defn same-letters [ids]
  (some (fn [idx]
          (->> ids
               (map (partial delete idx))
               (frequencies)
               (keep (fn [[k v]] (when (= 2 v) k)))
               seq))
        (range)))


(comment
  ;; First part
  (checksum data/box-ids)
  ;; Second part
  (same-letters data/box-ids))


;;;;;;;;;;
