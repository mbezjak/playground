(use '[clojure.test :only (is)])

(defn int->bit-vector [n]
  (letfn [(to-list [list x]
            (if (zero? x)
              list
              (to-list (conj list (mod x 2)) (quot x 2))))]
    (vec (to-list '() n))))

(is (= [1 0 1 0]
       (int->bit-vector 10)))

(defn pad-vector [length vector]
  (vec (concat (repeat (- length (count vector)) 0) vector)))

(is (= [1 1 1] (pad-vector 3 [1 1 1])))
(is (= [0 0 1 1 1] (pad-vector 5 [1 1 1])))

(defn pad-matrix [matrix]
  (let [max-length (apply max (map count matrix))]
    (mapv (partial pad-vector max-length) matrix)))

(defn bitmap->matrix [bitmap]
  (->> bitmap
       (map int->bit-vector)
       (pad-matrix)))

(def sample-matrix
  [[1 1 1 1]
   [1 1 1 1]
   [1 1 1 1]
   [1 1 1 1]])

(is (= sample-matrix
       (bitmap->matrix [15 15 15 15])))

(is (= [[1 1 1 1]
        [1 1 1 1]
        [1 1 1 1]
        [1 1 1 1]
        [1 1 1 1]]
       (bitmap->matrix [15 15 15 15 15])))

(is (= [[0 0 0 0 1]
        [0 0 0 1 1]
        [0 0 1 1 1]
        [0 1 1 1 1]
        [1 1 1 1 1]]
       (bitmap->matrix [1 3 7 15 31])))

(defn make-right-triangle [length]
  (for [x (range length)
        y (range length)
        :while (< y (- length x))]
    [x y]))

(defn make-oblique-triangle [height]
  (for [x (range height)
        y (range (dec (* 2 (inc x))))]
    [x (- y x)]))

(defn move-triangle [[point-x point-y] triangle]
  (map (fn [[x y]] [(+ x point-x) (+ y point-y)]) triangle))

(defn transpose-triangle [direction length triangle]
  (for [[x y] triangle]
    (condp = direction
      :vertical [x (- y (dec length))]
      :horizontal [(- x (dec length)) y]
      :vertical-oblique [(- x) y])))

(defn rotate-triangle-left [triangle]
  (for [[x y] triangle]
    [y x]))

(defn rotate-triangle-right [triangle]
  (for [[x y] triangle]
    [y (- x)]))

(defn oriented-triangle [orientation length]
  (condp = orientation
    ;; orientation for :xx is wrong
    :nw (make-right-triangle length)
    :ne (transpose-triangle :vertical length (make-right-triangle length))
    :sw (transpose-triangle :horizontal length (make-right-triangle length))
    :se (transpose-triangle :horizontal length (transpose-triangle :vertical length (make-right-triangle length)))
    :n  (make-oblique-triangle length)
    :s  (transpose-triangle :vertical-oblique length (make-oblique-triangle length))
    :w  (rotate-triangle-left (make-oblique-triangle length))
    :e  (rotate-triangle-right (make-oblique-triangle length))))

(defn triangle-area [length]
  (count (make-triangle length)))

(defn cross-section-values [matrix {:keys [initial-position orientation length]}]
  (->> (oriented-triangle orientation length)
       (move-triangle initial-position)
       (map #(get-in matrix %))))

(defn valid-cross-section? [matrix opts]
  (->> (cross-section-values matrix opts)
       (every? #(= 1 %))))

(is (valid-cross-section? sample-matrix {:initial-position [0 0] :orientation :nw :length 1}))
(is (valid-cross-section? sample-matrix {:initial-position [0 0] :orientation :nw :length 2}))
(is (valid-cross-section? sample-matrix {:initial-position [0 0] :orientation :nw :length 3}))
(is (not (valid-cross-section? sample-matrix {:initial-position [0 0] :orientation :se :length 2})))
(is (valid-cross-section? sample-matrix {:initial-position [0 2] :orientation :nw :length 2}))
(is (not (valid-cross-section? sample-matrix {:initial-position [0 2] :orientation :nw :length 3})))
(is (valid-cross-section? [[1 1 1] [0 1 1]] {:initial-position [0 0] :orientation :nw :length 1}))
(is (not (valid-cross-section? [[1 1 1] [0 1 1]] {:initial-position [0 0] :orientation :nw :length 2})))
(is (valid-cross-section? [[1 1 1] [0 1 1]] {:initial-position [1 1] :orientation :s :length 2}))

(defn all-cross-section-areas [matrix]
  (for [[row-idx columns] (map-indexed vector matrix)
        [col-idx value]   (map-indexed vector columns)
        :let [max-length (count columns)
              initial-position [row-idx col-idx]]
        orientation [:w :s :w :e :nw :ne :sw :se]
        length (range 1 (inc max-length))
        :when (= value 1)
        :while (valid-cross-section? matrix {:initial-position initial-position
                                             :orientation orientation
                                             :length length})]
    (do
      (println initial-position orientation length (oriented-triangle orientation length) (count (oriented-triangle orientation length)))
      (count (oriented-triangle orientation length)))))

(defn cross-section-area [bitmap]
  (if-let [areas (not-empty (all-cross-section-areas (bitmap->matrix bitmap)))]
    (apply max areas)))

(is (= 10 (cross-section-area [15 15 15 15 15])))
;; 1111      1111
;; 1111      *111
;; 1111  ->  **11
;; 1111      ***1
;; 1111      ****

(is (= 15 (cross-section-area [1 3 7 15 31])))
;; 00001      0000*
;; 00011      000**
;; 00111  ->  00***
;; 01111      0****
;; 11111      *****

(is (= 3 (cross-section-area [3 3])))
;; 11      *1
;; 11  ->  **

(is (= 4 (cross-section-area [7 3])))
;; 111      ***
;; 011  ->  0*1

(is (= 6 (cross-section-area [17 22 6 14 22])))
;; 10001      10001
;; 10110      101*0
;; 00110  ->  00**0
;; 01110      0***0
;; 10110      10110

(is (= 9 (cross-section-area [18 7 14 14 6 3])))
;; 10010      10010
;; 00111      001*0
;; 01110      01**0
;; 01110  ->  0***0
;; 00110      00**0
;; 00011      000*1

(is (= nil (cross-section-area [21 10 21 10])))
;; 10101      10101
;; 01010      01010
;; 10101  ->  10101
;; 01010      01010

(is (= nil (cross-section-area [0 31 0 31 0])))
;; 00000      00000
;; 11111      11111
;; 00000  ->  00000
;; 11111      11111
;; 00000      00000
