(defn f [xs n]
  (mapcat (partial repeat n) xs))

(assert (= (f [1 2 3] 2) '(1 1 2 2 3 3)))
(assert (= (f [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
(assert (= (f [4 5 6] 1) '(4 5 6)))
(assert (= (f [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
(assert (= (f [44 33] 2) [44 44 33 33]))
