(require '[clojure.set])
(use '[clojure.test :only (is)])

(defn powerset [coll]
  (if (empty? coll)
    #{#{}}
    (let [x (first coll)
          xs (set (rest coll))
          ps-of-xs (powerset xs)]
      (clojure.set/union ps-of-xs (set (map #(conj % x) ps-of-xs))))))

(is (= (powerset #{1 :a}) #{#{1 :a} #{:a} #{} #{1}}))

(is (= (powerset #{}) #{#{}}))

(is (= (powerset #{1 2 3})
       #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}}))

(is (= (count (powerset (into #{} (range 10)))) 1024))
