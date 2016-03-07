(require '[clojure.pprint :as pp])

;; sqrt approximation
(defn average [x y]
  (/ (+ x y) 2.0))

(defn sqrt-improve [guess x]
  (average guess (/ x guess)))

(defn sqrt-stream [x]
  (letfn [(guesses []
            (lazy-cat [1.0] (map #(sqrt-improve % x) (guesses))))]
    (guesses)))

(pp/pprint (take 10 (sqrt-stream 2)))


;; pi approximation
(defn square [x]
  (* x x))

(defn partial-sums [coll]
  (lazy-seq
   (for [n (range)]
     (apply + (take (inc n) coll)))))

(defn pi-summands [n]
  (lazy-cat [(/ 1.0 n)]
            (map - (pi-summands (+ n 2)))))

(defn scale-stream [coll scale]
  (map #(* % scale) coll))

(def pi-stream (scale-stream (partial-sums (pi-summands 1)) 4))

(pp/pprint (take 10 pi-stream))

(defn euler-transform [[s0 s1 s2 :as s]]
  (lazy-cat [(- s2 (/ (square (- s2 s1))
                      (+ s0 (* -2 s1) s2)))]
            (euler-transform (rest s))))

(pp/pprint (take 10 (euler-transform pi-stream)))

(defn make-tableau [transform s]
  (lazy-cat [s] (make-tableau transform (transform s))))

(defn accelerated-sequence [transform s]
  (map first (make-tableau transform s)))

(pp/pprint (take 10 (accelerated-sequence euler-transform pi-stream)))
