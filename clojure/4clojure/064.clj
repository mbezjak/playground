(def f +)

(assert (= 15 (reduce f [1 2 3 4 5])))
(assert (= 0 (reduce f [])))
(assert (= 6 (reduce f 1 [2 3])))
