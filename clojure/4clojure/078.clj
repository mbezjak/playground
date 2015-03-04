(use '[clojure.test :only (is)])

(defn tramp [f & args]
  (let [retval (apply f args)]
    (if (fn? retval)
      (tramp retval)
      retval)))

(is (= (letfn [(triple [x] #(sub-two (* 3 x)))
               (sub-two [x] #(stop?(- x 2)))
               (stop? [x] (if (> x 50) x #(triple x)))]
         (tramp triple 2))
       82))

(is (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
               (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
         (map (partial tramp my-even?) (range 6)))
       [true false true false true false]))
