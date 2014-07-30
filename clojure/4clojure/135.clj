(defn infix
  ([x] x)
  ([l op r & exprs] (apply infix (conj exprs (op l r)))))

(defn infix2
  ([x] x)
  ([l op r & exprs] (apply (partial infix2 (op l r)) exprs)))

(assert (= 7  (infix2 2 + 5)))
(assert (= 42 (infix2 38 + 48 - 2 / 2)))
(assert (= 8  (infix2 10 / 2 - 1 * 2)))
(assert (= 72 (infix2 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9)))
