(defn f [start end]
  (if (< start end)
    (conj (f (inc start) end) start)
    nil))

(assert (= (f 1 4) '(1 2 3)))
(assert (= (f -2 2) '(-2 -1 0 1)))
(assert (= (f 5 8) '(5 6 7)))
