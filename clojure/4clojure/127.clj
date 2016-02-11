(use '[clojure.test :only (is)])

(defn int->bit-array [n])

(defn pad-array [coll len])

(defn ints->2d-bits [ints])

(def sample-2d-bits
  [[1 1 1 1]
   [1 1 1 1]
   [1 1 1 1]
   [1 1 1 1]])

(defn valid-cross-section? [2d-bits {:keys [initial-position orientation length]}]
  (every? #(= 1 %) (for [x [1 2 3]
                         y [1 2 3]]
                     (get-in 2d-bits [x y]))))

(defn all-cross-section-areas [2d-bits])

(defn cross-section-area [bitmap]
  (apply max (all-cross-section-areas (ints->2d-bits bitmap))))

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
