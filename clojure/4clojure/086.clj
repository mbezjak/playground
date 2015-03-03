(use '[clojure.test :only (is)])

(defn happy? [x]
  (letfn [(digits [n]
            (if (zero? n)
              []
              (conj (digits (quot n 10)) (mod n 10))))
          (square [n]
            (* n n))
          (sum-squared [n]
            (apply + (map square (digits n))))]
    (= 1 (last (take 1000 (iterate sum-squared x))))))

(is (= (happy? 7) true))
(is (= (happy? 986543210) true))
(is (= (happy? 2) false))
(is (= (happy? 3) false))
