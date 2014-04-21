(defn f [n]
  (apply * (range 1 (inc n))))

(assert (= (f 1) 1))
(assert (= (f 3) 6))
(assert (= (f 5) 120))
(assert (= (f 8) 40320))
