(def x 1)

(assert (= x (if-not false 1 0)))
(assert (= x (if-not nil 1 0)))
(assert (= x (if true 1 0)))
(assert (= x (if [] 1 0)))
(assert (= x (if [0] 1 0)))
(assert (= x (if 0 1 0)))
(assert (= x (if 1 1 0)))
