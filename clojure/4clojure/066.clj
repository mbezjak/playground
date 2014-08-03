(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (mod a b))))

(assert (= (gcd 2 4) 2))
(assert (= (gcd 10 5) 5))
(assert (= (gcd 5 7) 1))
(assert (= (gcd 1023 858) 33))
