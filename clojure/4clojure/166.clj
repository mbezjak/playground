(defn cmp [lt x y]
  (cond
   (lt x y) :lt
   (lt y x) :gt
   :else    :eq))

(assert (= :gt (cmp < 5 1)))
(assert (= :eq (cmp (fn [x y] (< (count x) (count y))) "pear" "plum")))
(assert (= :lt (cmp (fn [x y] (< (mod x 5) (mod y 5))) 21 3)))
(assert (= :gt (cmp > 0 2)))
