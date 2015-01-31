(defn f [xs]
  (reduce
   (fn [col x]
     (if (= x (last col))
       col
       (conj col x)))
   []
   xs))

(assert (= (apply str (f "Leeeeeerrroyyy")) "Leroy"))
(assert (= (f [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
(assert (= (f [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2])))
