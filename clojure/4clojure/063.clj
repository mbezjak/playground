(defn group [f xs]
  (let [group-map (fn [m x]
                    (let [key (f x)
                          prev (get m key [])
                          next (conj prev x)]
                      (assoc m key next)))]
    (reduce group-map {} xs)))

(assert (= (group #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]}))
(assert (= (group #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
           {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]}))
(assert (= (group count [[1] [1 2] [3] [1 2 3] [2 3]])
           {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]}))
