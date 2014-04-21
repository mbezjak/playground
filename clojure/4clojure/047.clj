(def x 4)

(assert (contains? #{4 5 6} x))
(assert (contains? [1 1 1 1 1] x))
(assert (contains? {4 :a 2 :b} x))

(assert (not (try (contains? '(1 2 4) x)
                  (catch IllegalArgumentException e false))))
