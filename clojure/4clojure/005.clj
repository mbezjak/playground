(def x '(1 2 3 4))

(assert (= x (conj '(2 3 4) 1)))
(assert (= x (conj '(3 4) 2 1)))
