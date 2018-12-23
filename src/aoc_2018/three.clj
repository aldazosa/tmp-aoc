(ns aoc-2018.three
  (:require [clojure.spec.alpha :as s]
            [aoc-2018.data :as data]))


(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def letter (set alphabet))
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
(def row-8 (vec (repeat 8 \.)))
(def board-8 (vec (repeat 8 row-8)))


(def test-claim "#123 @ 3,2: 5x4")
(def row-11 (vec (repeat 11 \.)))
(def board-11 (vec (repeat 11 row-11)))

(def claim (read-claim test-claim))

(defn claim-coords
  [{:keys [from-left from-top height width]}]
  (for [r (range from-top (+ from-top height))
        c (range from-left (+ from-left width))]
    [r c]))


(defn update-position
  ([claim-id board [row col :as pos]]
   (update-in board pos
              #(if (= \. %)
                 claim-id
                 \C)))
  ([board [row col :as pos]]
   (update-position \# board pos)))


(defn update-position-v
  ([claim-id board [row col :as pos]]
   (update-in board pos
              #(if (= \. %)
                 #{claim-id}
                 (conj % claim-id))))
  ([board [row col :as pos]]
   (update-position \# board pos)))


(defn lay-claim
  [board claim]
  (reduce (partial update-position (:claim-number claim))
          board
          (claim-coords claim)))


(defn lay-claim-v
  [board claim]
  (reduce (partial update-position-v (:claim-number claim))
          board
          (claim-coords claim)))


(defn print-board
  [board]
  (clojure.string/join \
                       (map #(apply str %) board)))

(defn example []
  (let [board board-8
        board (reduce (fn [b c] (lay-claim b c))
                      board
                      [claim-1 claim-2 claim-3])]
    (->> board
         ;; (reduce into)
         ;; (filter #(= \C %))
         ;; (count)
         )))


(defn example-2 []
  (let [board board-8
        board (reduce (fn [b c] (lay-claim-v b c))
                      board
                      [claim-1 claim-2 claim-3])]
    board))

(def claims (doall (map read-claim data/claims)))


(defn part-1 []
  (let [board (vec
                (repeat 1000
                        (vec (repeat 1000 \.))))
        board (reduce (fn [b c] (lay-claim b c))
                      board
                      claims)]
    (->> board
         (reduce into)
         (filter #(= \C %))
         count)))

(defn conflict-free
  [board claims]
  (take-while
    (fn [{:keys [claim-number]}]
      (some (fn [row]
              (some
                (fn [space]
                  (and (set? space)
                       (contains? space claim-number)
                       (> (count space) 1)))
                row))
            board))
    claims))


(defn foo [board]
  (map (fn [row]
         (set
           (remove (fn [space]
                     (or
                       (char? space)
                       (< (count space) 2)))
                   row)))
       board))


(defn baz
  [board]
  (reduce clojure.set/union
    (map (fn [row]
           (reduce
             (fn [acc space]
               (if (or
                     (char? space)
                     (< (count space) ( 2)))
                 acc
                 (into acc space)))
             #{}
             row))
         board)))


(defn bar [board]
  (reduce (fn [acc row]
            (let [conflicts (remove (fn [space] (or
                                                  (char? space)
                                                  (< (count space) 2)))
                                    row)]
              (if (seq conflicts)
                (do
                  (println (set conflicts))
                  (into acc (set conflicts)))
                acc)))
          #{}
    board))


(defn part-2 [board claims]
  (some (fn [x]
          (when (not (contains? (baz board) x))
            x))
        (map :claim-number claims)))


(defn claimed-board [size claims]
  (let [board (vec
                (repeat size
                        (vec (repeat size \.))))]
    (reduce (fn [b c] (lay-claim-v b c))
            board
            claims)))
