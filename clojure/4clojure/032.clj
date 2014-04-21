(defn f [xs]
  (mapcat (fn [x] (list x x)) xs))

(assert (= (f [1 2 3]) '(1 1 2 2 3 3)))
(assert (= (f [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
(assert (= (f [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
(assert (= (f [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4])))
