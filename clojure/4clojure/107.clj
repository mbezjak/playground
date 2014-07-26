(defn f [n]
  (fn [x]
    (int (Math/pow x n))))

(assert (= 256 ((f 2) 16),
               ((f 8) 2)))
(assert (= [1 8 27 64] (map (f 3) [1 2 3 4])))
(assert (= [1 2 4 8 16] (map #((f %) 2) [0 1 2 3 4])))
