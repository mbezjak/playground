(defn iter [f x]
  (cons x (lazy-seq (iter f (f x)))))

(assert (= (take 5 (iter #(* 2 %) 1)) [1 2 4 8 16]))
(assert (= (take 100 (iter inc 0)) (take 100 (range))))
(assert (= (take 9 (iter #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3]))))
