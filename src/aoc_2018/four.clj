(ns aoc-2018.four
  (:require [clojure.spec.alpha :as s]
            [aoc-2018.data :as data]))


(def q
  ["[1518-11-03 00:29] wakes up"
   "[1518-11-01 00:00] Guard #10 begins shift"
   "[1518-11-02 00:50] wakes up"
   "[1518-11-01 00:25] wakes up"
   "[1518-11-03 00:05] Guard #10 begins shift"
   "[1518-11-01 00:55] wakes up"
   "[1518-11-02 00:40] falls asleep"
   "[1518-11-01 23:58] Guard #99 begins shift"
   "[1518-11-05 00:03] Guard #99 begins shift"
   "[1518-11-04 00:46] wakes up"
   "[1518-11-03 00:24] falls asleep"
   "[1518-11-01 00:05] falls asleep"
   "[1518-11-05 00:55] wakes up"
   "[1518-11-01 00:30] falls asleep"
   "[1518-11-04 00:02] Guard #99 begins shift"
   "[1518-11-05 00:45] falls asleep"
   "[1518-11-04 00:36] falls asleep"])


(def digit (set "1234567890"))

(s/def ::number
  (s/& (s/+ digit)
       (s/conformer
         (fn [parsed]
           (Long/parseLong (apply str parsed)))
         (fn [out]
           (seq (str out))))))


(defmacro match-seq [c]
  (let [ks      (mapv keyword (repeatedly (count c) gensym))
        cat-kvs (mapcat vector ks (map hash-set c))]
    `(s/with-gen
       (s/&
         (s/cat ~@cat-kvs)
         (s/conformer
           (juxt ~@ks)
           (fn [out#]
             (zipmap ~ks out#))))
       (fn []
         (s/gen #{~c})))))


(s/def ::date
  (s/cat :br1 #{\[}
         :year ::number
         :d1 #{\-}
         :month ::number
         :d2 #{\-}
         :day ::number
         :sp1 #{\ }
         :hour ::number
         :colon #{\:}
         :minute ::number
         :br2 #{\]}
         :sp2 #{\ }))


(s/def ::entry
  (s/and
    string?
    (s/conformer seq)
    (s/cat :date
           ::date

           :action
           (s/alt
             :sleeps (match-seq "falls asleep")
             :wakes  (match-seq "wakes up")
             :begins (s/cat :guard (match-seq "Guard #")
                            :id ::number
                            :shift (match-seq " begins shift"))))))


(defn read-entry
  [s]
  (let [{date         :date
         [action m] :action} (s/conform ::entry s)]
    (merge (select-keys date [:day :hour :minute :month :year])
           {:action action}
           (when (= action :begins)
             {:id (:id m)}))))

(->> q
     sort
     (map read-entry))

;;

(defn our-keys [m]
  (select-keys m [:action :hour :minute :id]))

(->> q
     sort
     (map read-entry)
     (map our-keys)
     (partition-by #(= :begins (:action %)))
     (partition 2))

;; Luego un reduce o un group-by
