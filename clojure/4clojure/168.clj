(use '[clojure.test :only (is)])

(defn as-matrix
  ([f]
   (as-matrix f 0 0))

  ([f m n]
   (let [list-from (fn list-from [x]
                     (cons x (lazy-seq (list-from (inc x)))))
         rs (list-from m)
         cs (list-from n)]
     (map (fn [i] (map (fn [j] (f i j)) cs)) rs)))

  ([f m n s t]
   (take s (map #(take t %) (as-matrix f m n)))))

(defn as-matrix2
  ([f]
   (for [i (range)]
     (for [j (range)]
       (f i j))))

  ([f m n]
   (drop m (map #(drop n %) (as-matrix2 f))))

  ([f m n s t]
   (take s (map #(take t %) (as-matrix2 f m n)))))


(is (= (take 5 (map #(take 6 %) (as-matrix str)))
       [["00" "01" "02" "03" "04" "05"]
        ["10" "11" "12" "13" "14" "15"]
        ["20" "21" "22" "23" "24" "25"]
        ["30" "31" "32" "33" "34" "35"]
        ["40" "41" "42" "43" "44" "45"]]))

(is (= (take 6 (map #(take 5 %) (as-matrix str 3 2)))
       [["32" "33" "34" "35" "36"]
        ["42" "43" "44" "45" "46"]
        ["52" "53" "54" "55" "56"]
        ["62" "63" "64" "65" "66"]
        ["72" "73" "74" "75" "76"]
        ["82" "83" "84" "85" "86"]]))

(is (= (as-matrix * 3 5 5 7)
       [[15 18 21 24 27 30 33]
        [20 24 28 32 36 40 44]
        [25 30 35 40 45 50 55]
        [30 36 42 48 54 60 66]
        [35 42 49 56 63 70 77]]))

(is (= (as-matrix #(/ % (inc %2)) 1 0 6 4)
       [[1/1 1/2 1/3 1/4]
        [2/1 2/2 2/3 1/2]
        [3/1 3/2 3/3 3/4]
        [4/1 4/2 4/3 4/4]
        [5/1 5/2 5/3 5/4]
        [6/1 6/2 6/3 6/4]]))

(is (= (class (as-matrix (juxt bit-or bit-xor)))
       (class (as-matrix (juxt quot mod) 13 21))
       (class (lazy-seq))))

(is (= (class (nth (as-matrix (constantly 10946)) 34))
       (class (nth (as-matrix (constantly 0) 5 8) 55))
       (class (lazy-seq))))

(is (= (let [m 377 n 610 w 987
             check (fn [f s] (every? true? (map-indexed f s)))
             row (take w (nth (as-matrix vector) m))
             column (take w (map first (as-matrix vector m n)))
             diagonal (map-indexed #(nth %2 %) (as-matrix vector m n w w))]
         (and (check #(= %2 [m %]) row)
              (check #(= %2 [(+ m %) n]) column)
              (check #(= %2 [(+ m %) (+ n %)]) diagonal)))
       true))
