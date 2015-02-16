(use '[clojure.test :only (is)])

(defn f [string]
  (sort-by (fn [word] (.toLowerCase word)) (re-seq #"[a-zA-Z]+" string)))

(is (= (f "Have a nice day.")
       ["a" "day" "Have" "nice"]))

(is (= (f "Clojure is a fun language!")
       ["a" "Clojure" "fun" "is" "language"]))

(is (= (f "Fools fall for foolish follies.")
       ["fall" "follies" "foolish" "Fools" "for"]))
