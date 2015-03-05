(use '[clojure.test :only (is)])

(defn balanced? [n]
  (letfn [(digits [x]
            (if (zero? x)
              []
              (conj (digits (quot x 10)) (mod x 10))))
          (split-middle [xs]
            (let [len (count xs)
                  middle (quot len 2)
                  odd-lenght? (= 1 (mod len 2))
                  [left right] (split-at middle xs)]
              (if odd-lenght?
                [left (drop 1 right)]
                [left right])))]
    (let [[left right] (split-middle (digits n))]
      (= (reduce + left) (reduce + right)))))

(is (= true (balanced? 11)))
(is (= true (balanced? 121)))
(is (= false (balanced? 123)))
(is (= true (balanced? 0)))
(is (= false (balanced? 88099)))
(is (= true (balanced? 89098)))
(is (= true (balanced? 89089)))
(is (= (take 20 (filter balanced? (range)))
       [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101]))
