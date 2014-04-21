(defn f [xs]
  (-> xs reverse first))

(assert (= (f [1 2 3 4 5]) 5))
(assert (= (f '(5 4 3)) 3))
(assert (= (f ["b" "c" "d"]) "d"))
