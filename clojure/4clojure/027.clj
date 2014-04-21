(defn f [xs]
  (let [revfn (if (string? xs)
                clojure.string/reverse
                reverse)]
    (= xs (revfn xs))))

(assert (false? (f '(1 2 3 4 5))))
(assert (true? (f "racecar")))
(assert (true? (f [:foo :bar :foo])))
(assert (true? (f '(1 1 3 3 1 1))))
(assert (false? (f '(:a :b :c))))
