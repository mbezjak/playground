(defn f [xs]
  (if (empty? xs)
    nil
    (concat (f (rest xs)) (list (first xs)))))

(assert (= (f [1 2 3 4 5]) [5 4 3 2 1]))
(assert (= (f (sorted-set 5 7 2 7)) '(7 5 2)))
(assert (= (f [[1 2] [3 4] [5 6]]) [[5 6] [3 4] [1 2]]))
