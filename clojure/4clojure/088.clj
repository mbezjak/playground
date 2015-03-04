(require '[clojure.set])

(defn f [xs ys]
  (clojure.set/union (clojure.set/difference xs ys) (clojure.set/difference ys xs)))

(assert (= (f #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}))
(assert (= (f #{:a :b :c} #{}) #{:a :b :c}))
(assert (= (f #{} #{4 5 6}) #{4 5 6}))
(assert (= (f #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]}))
