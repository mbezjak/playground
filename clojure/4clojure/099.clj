(defn to-digits [ds n]
  (if (zero? n)
    ds
    (to-digits (conj ds (mod n 10)) (quot n 10))))

(defn f [a b]
  (to-digits '() (* a b)))


(assert (= (f 1 1) [1]))
(assert (= (f 99 9) [8 9 1]))
(assert (= (f 999 99) [9 8 9 0 1]))
