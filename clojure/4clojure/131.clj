(use '[clojure.test :only (is)])
(require '[clojure.set])

(defn has-same-sub-sum [& sets]
  (let [powerset (fn powerset [coll]
                   (if (empty? coll)
                     #{#{}}
                     (let [x (first coll)
                           xs (set (rest coll))
                           ps-of-xs (powerset xs)]
                       (clojure.set/union ps-of-xs (set (map #(conj % x) ps-of-xs))))))
        sums (map (comp set #(map (partial apply +) %) #(disj % #{}) powerset) sets)
        same-sums (apply clojure.set/intersection sums)]

    (not (empty? same-sums))))

(is (= true  (has-same-sub-sum #{-1 1 99}
                               #{-2 2 888}
                               #{-3 3 7777})))   ; ex. all sets have a subset which sums to zero

(is (= false (has-same-sub-sum #{1}
                               #{2}
                               #{3}
                               #{4})))

(is (= true  (has-same-sub-sum #{1})))

(is (= false (has-same-sub-sum #{1 -3 51 9}
                               #{0}
                               #{9 2 81 33})))

(is (= true  (has-same-sub-sum #{1 3 5}
                               #{9 11 4}
                               #{-3 12 3}
                               #{-3 4 -2 10})))

(is (= false (has-same-sub-sum #{-1 -2 -3 -4 -5 -6}
                               #{1 2 3 4 5 6 7 8 9})))

(is (= true  (has-same-sub-sum #{1 3 5 7}
                               #{2 4 6 8})))

(is (= true  (has-same-sub-sum #{-1 3 -5 7 -9 11 -13 15}
                               #{1 -3 5 -7 9 -11 13 -15}
                               #{1 -1 2 -2 4 -4 8 -8})))

(is (= true  (has-same-sub-sum #{-10 9 -8 7 -6 5 -4 3 -2 1}
                               #{10 -9 8 -7 6 -5 4 -3 2 -1})))
