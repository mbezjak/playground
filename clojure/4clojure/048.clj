(def x 6)

(assert (= x (some #{2 7 6} [5 6 7 8])))
(assert (= x (some #(when (even? %) %) [5 6 7 8])))
