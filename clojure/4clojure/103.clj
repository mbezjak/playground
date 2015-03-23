(use '[clojure.test :only (is)])
(require '[clojure.set])

(defn gen-k [k coll]
  (letfn [(combinations [xs]
            (if (empty? xs)
              #{#{}}
              (let [fst (first xs)
                    rst (rest xs)
                    comb-rst (combinations rst)]
                (clojure.set/union #{#{fst}} comb-rst (set (map #(conj % fst) comb-rst))))))]

    (set (filter #(= k (count %)) (combinations coll)))))

(is (= (gen-k 1 #{4 5 6}) #{#{4} #{5} #{6}}))

(is (= (gen-k 10 #{4 5 6}) #{}))

(is (= (gen-k 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}}))

(is (= (gen-k 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                             #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}}))

(is (= (gen-k 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}}))

(is (= (gen-k 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                             #{:a "abc"} #{:a "efg"} #{"abc" "efg"}}))
