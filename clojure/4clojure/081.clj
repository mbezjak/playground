(defn f [xs ys]
  (set (filter xs ys)))

(assert (= (f #{0 1 2 3} #{2 3 4 5}) #{2 3}))
(assert (= (f #{0 1 2} #{3 4 5}) #{}))
(assert (= (f #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d}))
