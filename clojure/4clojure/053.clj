(use '[clojure.test :only (is)])

(defn longest-sub-seq [coll]
  (let [is-increasing (fn [[x y]] (= (inc x) y))
        sub-seqs-from (fn sub-seqs-from [xs]
                        (if (empty? xs)
                          nil
                          (let [[sub rst] (split-with is-increasing xs)]
                            (cons sub (sub-seqs-from (drop-while (complement is-increasing) rst))))))
        pairs (map vector coll (rest coll))
        sub-seq-pairs (sub-seqs-from pairs)
        longest-pairs (apply (partial max-key count) sub-seq-pairs)
        longest (if (empty? longest-pairs) '() (cons (ffirst longest-pairs) (map second longest-pairs)))]
    longest))

(is (= (longest-sub-seq [1 0 1 2 3 0 4 5]) [0 1 2 3]))
(is (= (longest-sub-seq [5 6 1 3 2 7]) [5 6]))
(is (= (longest-sub-seq [2 3 3 4 5]) [3 4 5]))
(is (= (longest-sub-seq [7 6 5 4]) []))
