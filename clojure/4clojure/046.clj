(defn flip [f]
  (fn [& args]
    (apply f (reverse args))))

(assert (= 3 ((flip nth) 2 [1 2 3 4 5])))
(assert (= true ((flip >) 7 8)))
(assert (= 4 ((flip quot) 2 8)))
(assert (= [1 2 3] ((flip take) [1 2 3 4 5] 3)))
