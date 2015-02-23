(use '[clojure.test :only (is)])

(defn reducs
  ([f xs] (if (empty? xs)
            (list (f))
            (reducs f (first xs) (rest xs))))
  ([f val xs] (if (empty? xs)
                (list val)
                (let [new-val (f val (first xs))]
                  (cons val (lazy-seq (reducs f new-val (rest xs))))))))

(is (= (take 5 (reducs + (range))) [0 1 3 6 10]))
(is (= (reducs conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]]))
(is (= (last (reducs * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120))
