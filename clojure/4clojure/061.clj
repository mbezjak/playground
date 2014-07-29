(defn f [xs ys]
  (apply assoc {}
         (interleave xs ys)))

(assert (= (f [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3}))
(assert (= (f [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"}))
(assert (= (f [:foo :bar] ["foo" "bar" "baz"])) {:foo "foo", :bar "bar"})
