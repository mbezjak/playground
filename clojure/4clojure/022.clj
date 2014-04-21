(defn f [xs]
  (reduce (fn [n v] (inc n)) 0 xs))

(assert (= (f '(1 2 3 3 1)) 5))
(assert (= (f "Hello World") 11))
(assert (= (f [[1 2] [3 4] [5 6]]) 3))
(assert (= (f '(13)) 1))
(assert (= (f '(:a :b :c)) 3))
