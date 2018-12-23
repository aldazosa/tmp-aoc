(ns )

;;;;;;;
;; 2 ;;
;;;;;;;

(defn diff-chars [r s]
  (map not=
    r
    s))

(defn same-chars [r s]
  (map = r s))

(defn extract-same [s same-chars])

(defn differences [x]
  (apply + x))


(defn foo [r s]
  (when-let [resultado (reduce (fn [acc [c d]]
                                 (if (= c d)
                                   (update acc :val conj c)
                                   (if (pos? (:errors acc))
                                     (reduced nil)
                                     (assoc acc :errors 1))))
                               {:errors 0
                                :val    []}
                               (map vector r s))]
    (apply str (:val resultado))))




(def second-part-ids ["abcde"
                      "fghij"
                      "klmno"
                      "pqrst"
                      "fguij"
                      "axcye"
                      "wvxyz"])

(defn pairs
  [ids]
  (into #{}
        (for [x ids
              y ids :when (not= x y)]
          #{x y})))

(defn same-chars
  [r s]
  (map = r s))

(defn same-chars3
  [r s]
  [r s
   (map #(if (= % %2) 0 1) r s)])

(defn same-chars4
  [r s]
  (map #(if (= % %2) 0 1) r s))


(for [p     (pairs data/box-ids)
      :let  [matches (apply same-chars4 p)]
      :when (= 1 (apply + matches))]
  [p matches])

(defn search-candidate-boxes
  [ids]
  (for [p     (pairs ids)
        :let  [matches (apply same-chars4 p)]
        :when (= 1 (apply + matches))]
    [p matches]))

(let [boxes (search-candidate-boxes data/box-ids)]
  (ffirst boxes))


(def xform
  (comp
    (map (fn [x]
           (let [[a b] (seq x)]
             [a b (same-chars4 a b)])))
    (filter (fn [[_ _ matches :as tuple]]
              (when (= 1 (apply + matches))
                tuple)))))

(def xxform
  (comp
    (map (fn [x]
           (let [[a b] (seq x)]
             [a b (same-chars4 a b)])))
    (halt-when (fn [[_ _ matches :as tuple]]
                 (when (= 1 (apply + matches))
                   tuple)))
    (filter (fn [[_ _ matches :as tuple]]
              (when (= 1 (apply + matches))
                tuple)))))


(time
  (into [] xform
        (pairs data/box-ids)))


(time
  (transduce xxform
             (completing (fn [acc x] (conj acc x)))
             []
             (pairs data/box-ids)))


(defn common-letters [ids]
  (when-let [[a _ m] (transduce xxform
                                (completing (fn [acc x] (conj acc x)))
                                []
                                (pairs ids))]
    (->> (map vector a m)
         (reduce (fn [acc [s match]]
                   (if (zero? match)
                     (conj acc s)
                     acc))
                 [])
         (apply str))))


(defn same-chars2
  [r s]
  (reduce (fn [acc [x y]]
            (cond

              (= x y)
              (update acc :val conj true)

              (pos? (:errors acc))
              (reduced nil)

              :else
              (-> acc
                  (update :val conj false)
                  (assoc :errors 1))))
          {:errors 0
           :val    []}
          (map vector r s)))


(defn idx-of-same-chars
  [r s]
  (reduce (fn [acc [idx in-both]]
            (if in-both
              (conj acc idx)
              acc))
          []
          (map vector
               (range)
               (same-chars2 r s))))

(defn common-letters [pair]
  (->> (apply idx-of-same-chars (seq pair))
       (map (fn [idx] (nth (first pair) idx)))))

(defn search-common-letters
  [ids]
  (let [desired-size (dec (count (first ids)))]
    (->> (map common-letters (pairs ids))
         (filter #(= desired-size (count %)))
         first
         (apply str))))

;; (for [p (pairs ids)]
;;     (->> (seq p)
;;          (apply idx-of-same-chars)
;;          ;; (map (fn [idx] (nth (first p) idx)))
;;          ;; (apply str)
;;          ))

;; (let [ids          second-part-ids
;;       desired-size (dec (count (first ids)))
;;       pairs        (pairs ids)]
;;   (->> pairs
;;        (map common-letters)
;;        (filter #(= desired-size (count %)))
;;        first
;;        (apply str)))
