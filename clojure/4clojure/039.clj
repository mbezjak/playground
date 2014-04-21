(defn f [xs ys]
  (cond
   (or (empty? xs) (empty? ys)) nil
   :else (concat (list (first xs) (first ys)) (f (rest xs) (rest ys)))))

(assert (= (f [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
(assert (= (f [1 2] [3 4 5 6]) '(1 3 2 4)))
(assert (= (f [1 2 3 4] [5]) [1 5]))
(assert (= (f [30 20] [25 15]) [30 25 20 15]))
