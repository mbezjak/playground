(defn f [xs]
  (mapcat (fn [e] (if (sequential? e) (f e) (list e))) xs))

(assert (= (f '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
(assert (= (f ["a" ["b"] "c"]) '("a" "b" "c")))
(assert (= (f '((((:a))))) '(:a)))
