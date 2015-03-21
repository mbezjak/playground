(use '[clojure.test :only (is)])

(defn intervals [coll]
  (cond
    (empty? coll) []
    (= 1 (count coll)) [[(first coll) (first coll)]]
    :else (let [adjacent-pairs (partition 2 1 (sort coll))
                [close-pairs rst-pairs] (split-with (fn [[x y]] (<= (- y x) 1)) adjacent-pairs)
                span (if (empty? close-pairs)
                       [(ffirst rst-pairs) (ffirst rst-pairs)]
                       [(ffirst close-pairs) (last (last close-pairs))])
                rst (map last rst-pairs)]
            (cons span (intervals rst)))))

(is (= (intervals [1 2 3]) [[1 3]]))

(is (= (intervals [10 9 8 1 2 3]) [[1 3] [8 10]]))

(is (= (intervals [1 1 1 1 1 1 1]) [[1 1]]))

(is (= (intervals []) []))

(is (= (intervals [19 4 17 1 3 10 2 13 13 2 16 4 2 15 13 9 6 14 2 11])
       [[1 4] [6 6] [9 11] [13 17] [19 19]]))
