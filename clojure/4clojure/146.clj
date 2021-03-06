(defn f [m]
  (apply merge (for [[k1 inner-map] m
                        [k2 val] inner-map
                        :let [path [k1 k2]]]
                 {path val})))

(assert (= (f '{a {p 1, q 2}
                 b {m 3, n 4}})
           '{[a p] 1, [a q] 2
             [b m] 3, [b n] 4}))

(assert (= (f '{[1] {a b c d}
                 [2] {q r s t u v w x}})
           '{[[1] a] b, [[1] c] d,
             [[2] q] r, [[2] s] t,
             [[2] u] v, [[2] w] x}))

(assert (= (f '{m {1 [a b c] 3 nil}})
           '{[m 1] [a b c], [m 3] nil}))
