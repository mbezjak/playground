(use '[clojure.test :only (is)])
(require '[clojure.set])

(defn parens [n]
  (if (<= n 0)
    #{""}
    (let [other (parens (dec n))
          from-left (set (map #(str "()" %) other))
          short-circuit (set (for [len-short (range (dec n) 0 -1)
                                   :let [len-rst (- n 1 len-short)]
                                   short (parens len-short)
                                   rst (parens len-rst)]
                               (str "(" short ")" rst)))]
      (clojure.set/union from-left short-circuit))))


(is (= [#{""} #{"()"} #{"()()" "(())"}] (map (fn [n] (parens n)) [0 1 2])))
(is (= #{"((()))" "()()()" "()(())" "(())()" "(()())"} (parens 3)))

(is (= (parens 4)
       (clojure.set/union #{"()()()()"}
                          #{"()()(())" "()(())()" "(())()()"}
                          #{"()((()))" "()(()())" "((()))()" "(()())()" "(())(())"}
                          #{"(()()())" "(()(()))" "((())())" "((()()))" "(((())))"})))

(is (= 16796 (count (parens 10))))
(is (= (nth (sort (filter #(.contains ^String % "(()()()())") (parens 9))) 6) "(((()()()())(())))"))
(is (= (nth (sort (parens 12)) 5000) "(((((()()()()()))))(()))"))
