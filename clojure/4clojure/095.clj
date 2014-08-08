(defn binary? [tree]
  (cond
      (nil? tree) true
      (or (not (sequential? tree)) (not= (count tree) 3)) false
      :else
      (let [[v l r] tree]
        (boolean (and (not (sequential? v))
                      (binary? l)
                      (binary? r))))))

(assert (= (binary? '(:a (:b nil nil) nil))
           true))

(assert (= (binary? '(:a (:b nil nil)))
           false))

(assert (= (binary? [1 nil [2 [3 nil nil] [4 nil nil]]])
           true))

(assert (= (binary? [1 [2 nil nil] [3 nil nil] [4 nil nil]])
           false))

(assert (= (binary? [1 [2 [3 [4 nil nil] nil] nil] nil])
           true))

(assert (= (binary? [1 [2 [3 [4 false nil] nil] nil] nil])
           false))

(assert (= (binary? '(:a nil ()))
           false))
