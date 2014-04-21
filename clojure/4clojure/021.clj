(defn f [xs n]
  (if (empty? xs)
    nil
    (->> xs (drop n) first)))

(assert (= (f '(4 5 6 7) 2) 6))
(assert (= (f [:a :b :c] 0) :a))
(assert (= (f [1 2 3 4] 1) 2))
(assert (= (f '([1 2] [3 4] [5 6]) 2) [5 6]))
