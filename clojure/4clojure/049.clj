(defn f [n xs]
  [(take n xs) (drop n xs)])

(assert (= (f 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
(assert (= (f 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
(assert (= (f 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]]))
