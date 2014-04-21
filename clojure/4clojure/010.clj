(def x 20)

(assert (= x ((hash-map :a 10, :b 20, :c 30) :b)))
(assert (= x (:b {:a 10, :b 20, :c 30})))
