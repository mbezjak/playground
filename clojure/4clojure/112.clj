(use '[clojure.test :only (is)])

(defn nested-sum [max coll]
  (letfn [(run [sum ints]
            (if (empty? ints)
              [sum nil]
              (let [[fst & rst] ints]
                (if (number? fst)
                  (let [next-sum (+ sum fst)]
                    (if (<= next-sum max)
                      (let [[after-rst-sum after-rst-ints] (run next-sum rst)]
                        [after-rst-sum (cons fst after-rst-ints)])
                      [sum nil]))
                  (let [[after-fst-sum after-fst-ints] (run sum fst)]
                    (if (<= after-fst-sum max)
                      (let [[after-rst-sum after-rst-ints] (run after-fst-sum rst)]
                        [after-rst-sum (cons after-fst-ints after-rst-ints)])
                      [sum nil]))))))]

    (or (second (run 0 coll)) '())))

(is (=  (nested-sum 10 [1 2 [3 [4 5] 6] 7])
        '(1 2 (3 (4)))))

(is (=  (nested-sum 30 [1 2 [3 [4 [5 [6 [7 8]] 9]] 10] 11])
        '(1 2 (3 (4 (5 (6 (7))))))))

(is (=  (nested-sum 9 (range))
        '(0 1 2 3)))

(is (=  (nested-sum 1 [[[[[1]]]]])
        '(((((1)))))))

(is (=  (nested-sum 0 [1 2 [3 [4 5] 6] 7])
        '()))

(is (=  (nested-sum 0 [0 0 [0 [0]]])
        '(0 0 (0 (0)))))

(is (=  (nested-sum 1 [-10 [1 [2 3 [4 5 [6 7 [8]]]]]])
        '(-10 (1 (2 3 (4))))))
