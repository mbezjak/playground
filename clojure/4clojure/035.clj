(def v 7)

(assert (= v (let [x 5] (+ 2 x))))
(assert (= v (let [x 3, y 10] (- y x))))
(assert (= v (let [x 21] (let [y 3] (/ x y)))))
