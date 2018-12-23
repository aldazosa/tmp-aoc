(ns )




;; another

(def bboard (repeat (* 1000 1000) \.))

(defn update-position-2
  [f b [row col :as pos]]
  (update b (f row col)
          #(if (= \. %)
             \#
             \C)))

(defn lay-claim-2
  [size board claim]
  (let [f                 #(+ (* size %2) %)
        update-position-2 (partial update-position-2 f)]
    (reduce update-position-2
            board
            (claim-coords claim))))

(defn print-s
  [s b]
  (print-board
    (partition s b)))


(defn part-11 []
  (let [board     (vec (repeat (* 1000 1000) \.))
        lay-claim (partial lay-claim-2 1000)
        board     (reduce (fn [b c] (lay-claim b c))
                          board
                          claims)]
    (->> board
         (filter #(= \C %))
         count)))


(defn lay-claim-3
  [size board claim]
  (let [f                 #(+ (* size %2) %)
        update-position-2 (partial update-position-2 f)]
    (reduce (fn [b [row col]]
              (let [pos (+ (* size row) col)
                    v   (get b pos)]
                (assoc! b pos (if (= \. v) \# \C))))
            board
            (claim-coords claim))))


(defn part-111 []
  (let [board     (vec (repeat (* 1000 1000) \.))
        lay-claim (partial lay-claim-3 1000)
        board     (persistent!
                    (reduce (fn [b c] (lay-claim b c))
                            (transient board)
                            claims))]
    (->> board
         (filter #(= \C %))
         count)))
