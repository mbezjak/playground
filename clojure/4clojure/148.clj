(use '[clojure.test :only (is)])

; http://www.algebra.com/algebra/homework/Sequences-and-series/Sequences-and-series.faq.question.427328.html
(defn sum [n a b]
  (letfn [(solve-series-for [diff]
            (let [upto (dec n)
                  length (quot upto diff)
                  first diff
                  last (- upto (mod upto diff))]
              (* (/ length 2) (+ first last))))]
    (+ (solve-series-for a) (solve-series-for b) (- (solve-series-for (* a b))))))

(defn sum2 [n a b]
  (apply + (set (concat (range 0 n a) (range 0 n b)))))

(defn sum3 [n a b]
  (letfn [(divisible [x y] (= 0 (mod x y)))]
    (apply + (filter #(or (divisible % a) (divisible % b)) (range n)))))


(is (= 0 (sum 3 17 11)))

(is (= 23 (sum 10 3 5)))

(is (= 233168 (sum 1000 3 5)))

(is (= "2333333316666668" (str (sum 100000000 3 5))))

(is (= "110389610389889610389610"
       (str (sum (* 10000 10000 10000) 7 11))))

(is (= "1277732511922987429116"
       (str (sum (* 10000 10000 10000) 757 809))))

(is (= "4530161696788274281"
       (str (sum (* 10000 10000 1000) 1597 3571))))
