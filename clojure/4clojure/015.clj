(defn f [x] (* x 2))
(def f' (partial * 2))
(def f'' (fn [x] (* x 2)))

(defn verify [fn]
  (assert (= (fn 2) 4))
  (assert (= (fn 3) 6))
  (assert (= (fn 11) 22))
  (assert (= (fn 7) 14)))

(verify f)
(verify f')
(verify f'')
