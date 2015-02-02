(defn f [n]
  (let [pascals-row  (fn pascals-row [m prev]
                       (if (= m n)
                         prev
                         (let [next (concat [1] (map #(apply + %) (map vector prev (drop 1 prev))) [1])]
                           (pascals-row (inc m) next))))]

    (pascals-row 1 [1])))

(assert (= (f 1) [1]))

(assert (= (map f (range 1 6))
   [     [1]
        [1 1]
       [1 2 1]
      [1 3 3 1]
     [1 4 6 4 1]]))

(assert (= (f 11)
           [1 10 45 120 210 252 210 120 45 10 1]))
