(use '[clojure.test :only (is)])

(defn f [n]
  (let [divisors (filter #(= 0 (mod n %)) (range 1 n))]
    (= n (reduce + divisors))))

(is (= (f 6) true))
(is (= (f 7) false))
(is (= (f 496) true))
(is (= (f 500) false))
(is (= (f 8128) true))
