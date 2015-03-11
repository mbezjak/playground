(use '[clojure.test :only (is)])

(defn oscilrate [init-val & fns]
  (let [intermediate-vals (reductions #(%2 %1) init-val fns)
        last-val (last intermediate-vals)]
    (lazy-cat intermediate-vals (rest (apply (partial oscilrate last-val) fns)))))

(is (= (take 3 (oscilrate 3.14 int double)) [3.14 3 3.0]))
(is (= (take 5 (oscilrate 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7]))
(is (= (take 12 (oscilrate 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3]))
