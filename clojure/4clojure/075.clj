(use '[clojure.test :only (is)])

(defn totient [x]
  (letfn [(gcd [a b]
            (if (zero? b)
              a
              (recur b (mod a b))))
          (coprime [a b]
            (= 1 (gcd a b)))]
    (if (= 1 x)
      1
      (count (filter (partial coprime x) (range 0 x))))))

(is (= (totient 1) 1))
(is (= (totient 10) (count '(1 3 7 9)) 4))
(is (= (totient 40) 16))
(is (= (totient 99) 60))
