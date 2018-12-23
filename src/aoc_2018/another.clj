(ns aoc-2018.three
  (:require [clojure.spec.alpha :as s]
            [aoc-2018.data :as data]
            [clojure.data.int-map :as i]))


(def digit (set "1234567890"))

(s/def ::number
  (s/& (s/+ digit)
       (s/conformer
         (fn [parsed]
           (Long/parseLong (apply str parsed)))
         (fn [out]
           (seq (str out))))))


(s/def ::claim
  (s/and string?
         (s/conformer seq)
         (s/cat :tag #{\#}
                :claim-number ::number
                :sp1 #{\ }
                :at #{\@}
                :sp2 #{\ }
                :from-left ::number
                :comma #{\,}
                :from-top  ::number
                :colon #{\:}
                :sp3 #{\ }
                :width ::number
                :x #{\x}
                :height ::number)))


(defn read-claim [s]
  (select-keys
    (s/conform ::claim s)
    [:claim-number :from-left :from-top :width :height]))


(def claim-1 (read-claim "#1 @ 1,3: 4x4"))
(def claim-2 (read-claim "#2 @ 3,1: 4x4"))
(def claim-3 (read-claim "#3 @ 5,5: 2x2"))

(defn claim-coords
  [size {:keys [from-left from-top height width]}]
  (for [r (range (* size from-top) (+ (* size from-top) height))
        c (range from-left (+ from-left width))]
    [r c]))

;; {:from-left 1, :from-top 3, :height 4, :width 5}
;; 25 26 27 28 29 (take 5 (range (+ (* 8 3) 1)))
;; 33 34 35 36 37 (range (* 8 4) (+ (* 8 4) 5 1))
;; 41 42 42 44 38 (range (* 8 5) (+ (* 8 5) 5 1))
;; 49 50 51 52 53 (range (* 8 6) (+ (* 8 6) 5 1))

;; {:from-left 3, :from-top 1, :height 4, :width 4}
;; 11 12 13 14 (take 4 (range (+ ())))
;; 19 20 21 22
;; 27 28 29 30
;; 35 36 37 38

(defn claim-coords
  [size {:keys [from-left from-top height width]}]
  (let [gen-fn #(take width
                      (drop (+ (* size (+ from-top %)) from-left)
                            (range)))]
    (map gen-fn
         (range height))))


(defn cc
  [size {:keys [from-left from-top height width]}]
  (let [gen-fn #(take width
                      (drop (+ (* size (+ from-top %)) from-left)
                            (range)))]
    (map gen-fn
         (range height))))


(defn ccc
  [size {:keys [from-left from-top height width]}]
  (let [gen-fn #(set
                  (take width
                        (drop (+ (* size (+ from-top %)) from-left)
                              (range))))]
    (loop [height height
           result #{}]
      (if-not (zero? height)
        (recur (dec height) (into result (gen-fn height)))
        result))))


(defn example []
  (let [claim-coords (partial claim-coords 8)
        claims (-> []
                   (into (claim-coords claim-1))
                   (into (claim-coords claim-2))
                   (into (claim-coords claim-3)))]
    (->> claims
         frequencies
         (filter (fn [[k v]]
                   (when (= v 2)
                     [k v])))
         count)))

(def claims (doall (map read-claim data/claims)))

(defn foo
  []
  (let [taken (reduce (fn [acc c]
                        (update acc
                                (claim-coords 1000 c)
                                (fnil inc 0)))
                      {}
                      (take 1 claims))]
    (->> taken
         ;; frequencies
         ;; (filter (fn [[k v]]
         ;;           (when (= v 2)
         ;;             [k v])))
         count
         )))


(defn foo
  []
  (reduce (fn [acc c]
            (reduce (fn [m [k v]]
                      (assoc m k v))
              acc
              (map vector (cc 1000 c) (repeat 1))))
          {}
          claims))
