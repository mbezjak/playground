(defn f [xs]
  (map vector xs (range)))

(assert (= (f [:a :b :c]) [[:a 0] [:b 1] [:c 2]]))
(assert (= (f [0 1 3]) '((0 0) (1 1) (3 2))))
(assert (= (f [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]]))
