(defn f [xs]
  (reduce + 0 xs))

(assert (= (f [1 2 3]) 6))
(assert (= (f (list 0 -2 5 5)) 8))
(assert (= (f #{4 2 1}) 7))
(assert (= (f '(0 0 -1)) -1))
(assert (= (f '(1 10 3)) 14))
