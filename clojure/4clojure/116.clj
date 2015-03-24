(use '[clojure.test :only (is)])

(defn balanced-prime? [n]
  (letfn [(prime? [x]
            (if (<= x 1)
              false
              (let [upper-bound (inc (int (Math/sqrt x)))]
                (every? #(not= (mod x %) 0) (range 2 upper-bound)))))
          (prime-below [x]
            (some #(if (prime? %) %) (range (dec x) 0 -1)))
          (prime-above [x]
            (some #(if (prime? %) %) (range (inc x) Integer/MAX_VALUE 1)))
          (avg [x y]
            (/ (+ x y) 2))]

    (let [below (prime-below n)
          above (prime-above n)]
      (and (prime? n) below above (= n (avg below above))))))


(is (= false (balanced-prime? 4)))
(is (= true (balanced-prime? 563)))
(is (= 1103 (nth (filter balanced-prime? (range)) 15)))
