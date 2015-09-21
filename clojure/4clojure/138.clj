(use '[clojure.test :only (is)])

(defn squares [start end]
  (take-while #(<= % end) (iterate #(* % %) start)))

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

(defn place-digit [shape digit]
  (if (full? shape)
    (place-digit (grow shape) digit)
    ;; what?
    ))

;;    1  digits -> 1x1
;;  2-4  digits -> 3x3
;;  5-9  digits -> 5x5
;; 10-16 digits -> 7x7


(defn shape [start end]
  (let [ds (digits (squares start end))]
    (reduce place-digit ["*"] ds)))

ee
(is (= (shape 2 2) ["2"]))

(is (= (shape 2 4) [" 2 "
                    "* 4"
                    " * "]))

(is (= (shape 3 81) [" 3 "
                     "1 9"
                     " 8 "]))

(is (= (shape 4 20) [" 4 "
                     "* 1"
                     " 6 "]))

(is (= (shape 2 256) ["  6  "
                      " 5 * "
                      "2 2 *"
                      " 6 4 "
                      "  1  "]))

(is (= (shape 10 10000) ["   0   "
                         "  1 0  "
                         " 0 1 0 "
                         "* 0 0 0"
                         " * 1 * "
                         "  * *  "
                         "   *   "]))
