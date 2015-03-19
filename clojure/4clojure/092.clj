(use '[clojure.test :only (is)])

(defn from-roman [roman]
  (let [letter-nums {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}
        nums (map letter-nums roman)
        with-rest (map-indexed (fn [idx num] [num (drop (inc idx) nums)]) nums)]
    (reduce (fn [sum [n others]]
              (if (some (partial < n) others)
                (- sum n)
                (+ sum n)))
            0
            with-rest)))

(is (= 14 (from-roman "XIV")))
(is (= 827 (from-roman "DCCCXXVII")))
(is (= 3999 (from-roman "MMMCMXCIX")))
(is (= 48 (from-roman "XLVIII")))
