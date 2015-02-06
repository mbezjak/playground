(defn f [begin-row]
  (iterate (fn [row]
             (let [fst (first row)
                   lst (if (>= (count row) 2) (last row) fst)
                   next-row (concat [fst] (map +' row (drop 1 row)) [lst])]
               next-row))

           begin-row))

(assert (= (second (f [2 3 2])) [2 5 5 2]))
(assert (= (take 5 (f [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]]))
(assert (= (take 2 (f [3 1 2])) [[3 1 2] [3 4 3 2]]))
(assert (= (take 100 (f [2 4 2])) (rest (take 101 (f [2 2])))))
