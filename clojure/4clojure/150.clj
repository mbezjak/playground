(use '[clojure.test :only (is)])

(defn palindroms [n]
  (if (< n 10)
    (cons n (palindroms (inc n)))
    (let [s (str n)
          len (count s)
          split (fn []
                  (let [[l-coll r-coll] (split-at (quot len 2) s)
                        l (apply str l-coll)
                        r (apply str r-coll)]
                    (if (even? len)
                      [l nil r]
                      [l (str (first r)) (rest r)])))
          [left pivot right] (split)
          palindrom (read-string (str left (or pivot "") (apply str (reverse left))))
          zeros (apply str (map (constantly 0) left))
          next-num (read-string (str (inc (read-string (if pivot (str left pivot) left)))
                                     zeros))]

      (if (< palindrom n)
        (lazy-seq (palindroms next-num))
        (lazy-seq (cons palindrom (palindroms next-num)))))))

(defn palindroms2 [n]
  (if (< n 10)
    (cons n (palindroms2 (inc n)))
    (let [pow10 (fn [e] (bigint (Math/pow 10 e)))
          digit-at (fn [d idx] (int (.remainder (.movePointLeft d idx) (bigdec 10))))
          reverse-of (fn [d]
                       (let [p (.precision d)]
                         (reduce (fn [n idx]
                                   (+ n (* (digit-at d (bigdec (- p idx 1))) (pow10 idx))))
                                 0
                                 (range p))))
          dec (bigdec n)
          precision (.precision dec)
          first-half-precision (quot precision 2)
          first-half (bigint (.movePointLeft dec (- precision first-half-precision)))
          pivot (if (odd? precision) (digit-at dec first-half-precision) nil)
          without-last-half (+ (* first-half (pow10 (+ first-half-precision (if pivot 1 0))))
                               (if pivot (* pivot (pow10 first-half-precision)) 0))
          palindrom (+ without-last-half (reverse-of (bigdec first-half)))
          next-num (+ without-last-half (pow10 first-half-precision))]

      (if (< palindrom n)
        (lazy-seq (palindroms2 next-num))
        (lazy-seq (cons palindrom (palindroms2 next-num)))))))

(defn palindroms3 [n]
  (letfn [(digits [x]
            (if (zero? x)
              []
              (conj (digits (quot x 10)) (mod x 10))))
          (join [left pivot right]
            (concat left (if pivot [pivot] []) right))
          (to-num [ds]
            (apply + (map-indexed (fn [idx d] (* (pow10 idx) d)) ds)))
          (pow10 [e]
            (bigint (Math/pow 10 e)))

          (start-from [num]
            (if (< num 10)
              (lazy-seq (cons num (start-from (inc num))))
              (let [ds (digits num)
                    len (count ds)
                    pivot (if (odd? len) (nth ds (quot len 2)) nil)
                    first-half-len (quot len 2)
                    first-half-digits (take first-half-len ds)
                    palindrom (to-num (join first-half-digits pivot (reverse first-half-digits)))
                    next-num (+ num (pow10 first-half-len))]
                (lazy-seq (cons palindrom (start-from next-num))))))]

    (drop-while #(< % n) (start-from n))))


(is (= (take 26 (palindroms 0))
       [0 1 2 3 4 5 6 7 8 9
        11 22 33 44 55 66 77 88 99
        101 111 121 131 141 151 161]))

(is (= (take 16 (palindroms 162))
       [171 181 191 202
        212 222 232 242
        252 262 272 282
        292 303 313 323]))

(is (= (take 6 (palindroms 1234550000))
       [1234554321 1234664321 1234774321
        1234884321 1234994321 1235005321]))

(is (= (first (palindroms (* 111111111 111111111)))
       (* 111111111 111111111)))

(is (= (set (take 199 (palindroms 0)))
       (set (map #(first (palindroms %)) (range 0 10000)))))

(is (= true
       (apply < (take 6666 (palindroms 9999999)))))

(is (= (nth (palindroms 0) 10101)
       9102019))
