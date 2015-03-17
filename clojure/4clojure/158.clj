(use '[clojure.test :only (is)])

(defn decurry [f]
  (fn [& args]
    (if (empty? args)
      (f)
      (reduce #(%1 %2) f args))))

(is (= 10 ((decurry (fn [a]
                      (fn [b]
                        (fn [c]
                          (fn [d]
                            (+ a b c d))))))
           1 2 3 4)))

(is (= 24 ((decurry (fn [a]
                      (fn [b]
                        (fn [c]
                          (fn [d]
                            (* a b c d))))))
           1 2 3 4)))

(is (= 25 ((decurry (fn [a]
                      (fn [b]
                        (* a b))))
           5 5)))
