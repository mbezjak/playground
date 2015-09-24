(use '[clojure.test :only (is)])

(defn square [x]
  (* x x))

(defn squares [start end]
  (take-while #(<= % end) (iterate square start)))

(is (= [2] (squares 2 2)))
(is (= [2 4] (squares 2 4)))
(is (= [3 9 81] (squares 3 81)))
(is (= [2 4 16 256] (squares 2 256)))

(defn digits [numbers]
  (vec (mapcat str numbers)))

(is (= [\2] (digits [2])))
(is (= [\2 \4] (digits [2 4])))
(is (= [\3 \9 \8 \1] (digits [3 9 81])))
(is (= [\2 \4 \1 \6 \2 \5 \6] (digits [2 4 16 256])))

;;    1  digits -> 1x1
;;  2-4  digits -> 3x3
;;  5-9  digits -> 5x5
;; 10-16 digits -> 7x7

(defn shape-dim [num-elems]
  (->> num-elems
       (Math/sqrt)
       (Math/round)
       (* 2)
       dec))

(is (= 1 (shape-dim 1)))
(is (= 3 (shape-dim 4)))
(is (= 5 (shape-dim 8)))
(is (= 7 (shape-dim 16)))

(defn pad [ds]
  (let [size (count ds)
        upper-limit (->> size
                         (Math/sqrt)
                         (Math/ceil)
                         (Math/round)
                         (square))]
    (vec (take upper-limit (concat ds (repeat \*))))))

(is (= [1 2 \* \*] (pad [1 2])))
(is (= [1 2 3 4] (pad [1 2 3 4])))

(defn empty-shape [dim]
  (vec (repeat dim (apply str (repeat dim \ )))))

;; 1-4   digits => index=0
;; 5-16  digits => index=2
;; 17-36 digits => index=4

(defn start-pos-y [size]
  (let [base (int (Math/sqrt size))
        even-base (square (if (odd? base) (inc base) base))]
    (- (int (Math/sqrt even-base)) 2)))

(is (= 0 (start-pos-y 1)))
(is (= 0 (start-pos-y 4)))
(is (= 2 (start-pos-y 9)))
(is (= 2 (start-pos-y 16)))

(defn start-pos)

(defn can-turn?)

(defn place-digit [shape digit]
  )

(defn make-shape [start end]
  (let [elems (pad (digits (squares start end)))
        shape (empty-shape (shape-dim (count elems)))]
    (reduce place-digit shape elems)))

ee
(is (= (make-shape 2 2) ["2"]))

(is (= (make-shape 2 4) [" 2 "
                         "* 4"
                         " * "]))

(is (= (make-shape 3 81) [" 3 "
                          "1 9"
                          " 8 "]))

(is (= (make-shape 4 20) [" 4 "
                          "* 1"
                          " 6 "]))

(is (= (make-shape 2 256) ["  6  "
                           " 5 * "
                           "2 2 *"
                           " 6 4 "
                           "  1  "]))

(is (= (make-shape 10 10000) ["   0   "
                              "  1 0  "
                              " 0 1 0 "
                              "* 0 0 0"
                              " * 1 * "
                              "  * *  "
                              "   *   "]))
