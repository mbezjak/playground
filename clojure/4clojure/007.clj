(def x [1 2 3 4])

(assert (= x (conj [1 2 3] 4)))
(assert (= x (conj [1 2] 3 4)))
