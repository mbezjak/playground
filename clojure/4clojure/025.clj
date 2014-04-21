(defn f [xs]
  (filter #(= 1 (rem % 2)) xs))

(assert (= (f #{1 2 3 4 5}) '(1 3 5)))
(assert (= (f [4 2 1 6]) '(1)))
(assert (= (f [2 2 4 6]) '()))
(assert (= (f [1 1 1 3]) '(1 1 1 3)))
