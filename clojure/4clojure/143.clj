(defn dot [xs ys]
  (apply + (map * xs ys)))

(assert (= 0 (dot [0 1 0] [1 0 0])))
(assert (= 3 (dot [1 1 1] [1 1 1])))
(assert (= 32 (dot [1 2 3] [4 5 6])))
(assert (= 256 (dot [2 5 6] [100 10 1])))
