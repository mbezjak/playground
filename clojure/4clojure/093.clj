(use '[clojure.test :only (is)])

(defn partial-flatten [coll]
  (letfn [(seq-of-things? [xs]
            (not-any? sequential? xs))
          (flat-to-vector [v el]
            (if (seq-of-things? el)
              (conj v el)
              (vec (concat v (partial-flatten el)))))]

    (reduce flat-to-vector [] coll)))

(is (= (partial-flatten [["Do"] ["Nothing"]])
       [["Do"] ["Nothing"]]))

(is (= (partial-flatten [[[[:a :b]]] [[:c :d]] [:e :f]])
       [[:a :b] [:c :d] [:e :f]]))

(is (= (partial-flatten '((1 2) ((3 4) ((((5 6)))))))
       '((1 2) (3 4) (5 6))))
