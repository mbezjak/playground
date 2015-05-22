(use '[clojure.test :only (is)])

                                        ; https://en.wikipedia.org/wiki/Levenshtein_distance

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

(defn levenshtein2 [s t]
  (lev-recursive s (count s) t (count t)))


(defn lev-matrix
  ([s m t n matrix] (lev-matrix s m t n matrix 1 1))
  ([s m t n matrix j i]
   (let [j-1 (dec j)
         i-1 (dec i)
         i+1 (inc i)]
     (cond
       (> j n) (get-in matrix [m n])
       (> i m) (lev-matrix s m t n matrix (inc j) 1)
       (= (get s i-1) (get t j-1)) (lev-matrix s m t n (assoc-in matrix [i j] (get-in matrix [i-1 j-1])) j i+1)
       :else
       (let [deletion     (inc (get-in matrix [i-1, j]))
             insertion    (inc (get-in matrix [i,   j-1]))
             substitution (inc (get-in matrix [i-1, j-1]))
             min-action (min deletion insertion substitution)]
         (lev-matrix s m t n (assoc-in matrix [i j] min-action) j i+1))))))

(defn lev-initial-matrix [m n]
  (vec (for [i (range (inc m))]
         (vec (for [j (range (inc n))]
                (cond
                  (zero? i) j
                  (zero? j) i
                  :else 0))))))

(is (= (lev-initial-matrix 3 3)
       [[0 1 2 3]
        [1 0 0 0]
        [2 0 0 0]
        [3 0 0 0]]))

(defn levenshtein [s t]
  (let [m (count s)
        n (count t)
        matrix (lev-initial-matrix m n)]
    (lev-matrix s m t n matrix)))


(defn levenshtein-4clojure [s t]
  (letfn [(lev-initial-matrix [m n]
            (vec (for [i (range (inc m))]
                   (vec (for [j (range (inc n))]
                          (cond
                            (zero? i) j
                            (zero? j) i
                            :else 0))))))
          (lev-matrix
            ([s m t n matrix] (lev-matrix s m t n matrix 1 1))
            ([s m t n matrix j i]
             (let [j-1 (dec j)
                   i-1 (dec i)
                   i+1 (inc i)]
               (cond
                 (> j n) (get-in matrix [m n])
                 (> i m) (lev-matrix s m t n matrix (inc j) 1)
                 (= (get s i-1) (get t j-1)) (lev-matrix s m t n (assoc-in matrix [i j] (get-in matrix [i-1 j-1])) j i+1)
                 :else
                 (let [deletion     (inc (get-in matrix [i-1, j]))
                       insertion    (inc (get-in matrix [i,   j-1]))
                       substitution (inc (get-in matrix [i-1, j-1]))
                       min-action (min deletion insertion substitution)]
                   (lev-matrix s m t n (assoc-in matrix [i j] min-action) j i+1))))))]
    (let [m (count s)
          n (count t)
          matrix (lev-initial-matrix m n)]
      (lev-matrix s m t n matrix))))


(is (= (levenshtein "kitten" "sitting") 3))
(is (= (levenshtein "closure" "clojure") (levenshtein "clojure" "closure") 1))
(is (= (levenshtein "xyx" "xyyyx") 2))
(is (= (levenshtein "" "123456") 6))
(is (= (levenshtein "Clojure" "Clojure") (levenshtein "" "") (levenshtein [] []) 0))
(is (= (levenshtein [1 2 3 4] [0 2 3 4 5]) 2))
(is (= (levenshtein '(:a :b :c :d) '(:a :d)) 2))
(is (= (levenshtein "ttttattttctg" "tcaaccctaccat") 10))
(is (= (levenshtein "gaattctaatctc" "caaacaaaaaattt") 9))
