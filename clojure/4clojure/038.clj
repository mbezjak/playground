(defn f [& xs]
  (last (sort xs)))

(assert (= (f 1 8 3 4) 8))
(assert (= (f 30 20) 30))
(assert (= (f 45 67 11) 67))
