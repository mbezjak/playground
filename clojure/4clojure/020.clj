(defn f [xs]
  (->> xs reverse (drop 1) (first)))

(assert (= (f (list 1 2 3 4 5)) 4))
(assert (= (f ["a" "b" "c"]) "b"))
(assert (= (f [[1 2] [3 4]]) [1 2]))
