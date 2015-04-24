(use '[clojure.test :only (is)])

(defn minsum [tree]
  (let [paths-with-indices (reduce (fn [paths siblings]
                                     (for [path paths
                                           [sidx sibling] (map-indexed vector siblings)
                                           :let [pidx (first (last path))]
                                           :when (or (= sidx pidx) (= sidx (inc pidx)))]
                                       (conj path [sidx sibling])))
                                   [[[0 0]]]
                                   tree)
        paths (map #(map second %) paths-with-indices)
        sums (map #(reduce + 0 %) paths)]
    (apply min sums)))

(is (= 7 (minsum '([1]
                  [2 4]
                 [5 1 4]
                [2 3 4 5]))))              ; 1->2->1->3

(is (= 20 (minsum '([3]
                   [2 4]
                  [1 9 3]
                 [9 9 2 4]
                [4 6 6 7 8]
               [5 7 3 5 1 4]))))    ; 3->4->3->2->7->1
