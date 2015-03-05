(use '[clojure.test :only (is)])

(defn equivalence [f d]
  (->> (group-by f d)
       (vals)
       (map set)
       set))

(is (= (equivalence #(* % %) #{-2 -1 0 1 2})
       #{#{0} #{1 -1} #{2 -2}}))

(is (= (equivalence #(rem % 3) #{0 1 2 3 4 5 })
       #{#{0 3} #{1 4} #{2 5}}))

(is (= (equivalence identity #{0 1 2 3 4})
       #{#{0} #{1} #{2} #{3} #{4}}))

(is (= (equivalence (constantly true) #{0 1 2 3 4})
       #{#{0 1 2 3 4}}))
