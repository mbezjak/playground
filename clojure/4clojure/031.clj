(defn f [xs]
  (if (empty? xs)
    xs
    (let [x (first xs)
          [same rest] (split-with #(= x %) xs)]
      (concat [same] (f rest)))))

(assert (= (f [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
(assert (= (f [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
(assert (= (f [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4]))))
