(use '[clojure.test :only (is)])

(defn f [xs]
  (vals (group-by class xs)))

(is (= (set (f [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]}))
(is (= (set (f [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]}))
(is (= (set (f [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]}))
