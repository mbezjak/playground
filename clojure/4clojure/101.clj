(use '[clojure.test :only (is)])

(defn lev-recursive [s len-s t len-t]
  (cond
    (zero? len-s) len-t
    (zero? len-t) len-s
    :else
    (let [len-s-1 (dec len-s)
          len-t-1 (dec len-t)
          cost (if (= (get s len-s-1) (get t len-t-1)) 0 1)]
      (min (+ 1    (lev-recursive s len-s-1 t len-t))
           (+ 1    (lev-recursive s len-s   t len-t-1))
           (+ cost (lev-recursive s len-s-1 t len-t-1))))))

(defn levenshtein [s t]
  (lev-recursive s (count s) t (count t)))

(is (= (levenshtein "kitten" "sitting") 3))
(is (= (levenshtein "closure" "clojure") (levenshtein "clojure" "closure") 1))
(is (= (levenshtein "xyx" "xyyyx") 2))
(is (= (levenshtein "" "123456") 6))
(is (= (levenshtein "Clojure" "Clojure") (levenshtein "" "") (levenshtein [] []) 0))
(is (= (levenshtein [1 2 3 4] [0 2 3 4 5]) 2))
(is (= (levenshtein '(:a :b :c :d) '(:a :d)) 2))
(is (= (levenshtein "ttttattttctg" "tcaaccctaccat") 10))
(is (= (levenshtein "gaattctaatctc" "caaacaaaaaattt") 9))
