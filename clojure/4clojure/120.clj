(defn digits [n]
  (if (zero? n)
    []
    (conj (digits (quot n 10)) (mod n 10))))

(defn square [x]
  (* x x))

(defn sum-squared [n]
  (apply + (map square (digits n))))

(defn f [xs]
  (let [lt-sum-squared (fn [n] (< n (sum-squared n)))]
    (count (filter lt-sum-squared xs))))

(assert (= 8 (f (range 10))))
(assert (= 19 (f (range 30))))
(assert (= 50 (f (range 100))))
(assert (= 50 (f (range 1000))))
