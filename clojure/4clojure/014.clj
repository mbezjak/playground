(def v 8)

(assert (= v ((fn add-five [x] (+ x 5)) 3)))
(assert (= v ((fn [x] (+ x 5)) 3)))
(assert (= v (#(+ % 5) 3)))
(assert (= v ((partial + 5) 3)))
