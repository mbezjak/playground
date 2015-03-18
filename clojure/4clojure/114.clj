(use '[clojure.test :only (is)])

(defn global-take-while [n p coll]
  (if (or (zero? n) (empty? coll))
    nil
    (let [[fst & rst] coll
          next-n (if (p fst) (dec n) n)]
      (if (zero? next-n)
        nil
        (cons fst (global-take-while next-n p rst))))))

(is (= [2 3 5 7 11 13]
       (global-take-while 4 #(= 2 (mod % 3))
                          [2 3 5 7 11 13 17 19 23])))

(is (= ["this" "is" "a" "sentence"]
       (global-take-while 3 #(some #{\i} %)
                          ["this" "is" "a" "sentence" "i" "wrote"])))

(is (= ["this" "is"]
       (global-take-while 1 #{"a"}
                          ["this" "is" "a" "sentence" "i" "wrote"])))

(is (= [0 1 2 3 4 5 6]
       (global-take-while 4 odd? (range))))
