(assert (= 10 (let [x 7, y 3, z 1] (+ x y))))
(assert (= 4 (let [x 7, y 3, z 1] (+ y z))))
(assert (= 1 (let [x 7, y 3, z 1] z)))
