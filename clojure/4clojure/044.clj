(defn f [n xs]
  (let [idx (mod n (count xs))
        [left right] (split-at idx xs)]
    (concat right left)))

(assert (= (f 2 [1 2 3 4 5]) '(3 4 5 1 2)))
(assert (= (f -2 [1 2 3 4 5]) '(4 5 1 2 3)))
(assert (= (f 6 [1 2 3 4 5]) '(2 3 4 5 1)))
(assert (= (f 1 '(:a :b :c)) '(:b :c :a)))
(assert (= (f -4 '(:a :b :c)) '(:c :a :b)))
