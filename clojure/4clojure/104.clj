(use '[clojure.test :only (is)])

(defn to-roman [num]
  (cond
    (zero? num) ""
    (>= num 1000) (str "M" (to-roman (- num 1000)))
    (>= num 900) (str "C" (to-roman (+ num 100)))
    (>= num 500) (str "D" (to-roman (- num 500)))
    (>= num 400) (str "C" (to-roman (+ num 100)))
    (>= num 100) (str "C" (to-roman (- num 100)))
    (>= num 90) (str "X" (to-roman (+ num 10)))
    (>= num 50) (str "L" (to-roman (- num 50)))
    (>= num 40) (str "X" (to-roman (+ num 10)))
    (>= num 10) (str "X" (to-roman (- num 10)))
    (>= num 9) (str "I" (to-roman (+ num 1)))
    (>= num 5) (str "V" (to-roman (- num 5)))
    (>= num 4) (str "I" (to-roman (+ num 1)))
    (>= num 1) (str "I" (to-roman (- num 1)))))

(is (= "I" (to-roman 1)))
(is (= "XXX" (to-roman 30)))
(is (= "IV" (to-roman 4)))
(is (= "CXL" (to-roman 140)))
(is (= "DCCCXXVII" (to-roman 827)))
(is (= "MMMCMXCIX" (to-roman 3999)))
(is (= "XLVIII" (to-roman 48)))
