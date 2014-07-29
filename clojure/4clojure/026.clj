(defn f1 [n]
  (seq
   (nth
    (iterate
     (fn [xs]
       (let [[a b] (take-last 2 xs)]
         (conj xs (+ a b))))
     [1 1])
    (- n 2))))

(defn f2 [n]
  (take n (map first (iterate (fn [[a b]] [b (+ a b)]) [1 1]))))

(defn f3 [n]
  (->> [1 1]
       (iterate (fn [[a b]] [b (+ a b)]))
       (map first)
       (take n)))

(assert (= (f3 3) '(1 1 2)))
(assert (= (f3 6) '(1 1 2 3 5 8)))
(assert (= (f3 8) '(1 1 2 3 5 8 13 21)))
