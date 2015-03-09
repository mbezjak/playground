(use '[clojure.test :only (is)])

(defn to-base [num base]
  (letfn [(loop [n b]
            (if (zero? n)
              []
              (conj (loop (quot n b) b) (mod n b))))]

    (if (zero? num)
      [0]
      (loop num base))))

(is (= [1 2 3 4 5 0 1] (to-base 1234501 10)))
(is (= [0] (to-base 0 11)))
(is (= [1 0 0 1] (to-base 9 2)))
(is (= [1 0] (let [n (rand-int 100000)](to-base n n))))
(is (= [16 18 5 24 15 1] (to-base Integer/MAX_VALUE 42)))
