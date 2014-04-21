(def x 3)

(assert (= x (first '(3 2 1))))
(assert (= x (second [2 3 4])))
(assert (= x (last (list 1 2 3))))
