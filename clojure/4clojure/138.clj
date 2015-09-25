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
  (vec (repeat dim (vec (repeat dim \ )))))

;; 1-4   digits => index=0
;; 5-16  digits => index=2
;; 17-36 digits => index=4

(defn start-pos-x [size]
  (let [base (int (Math/sqrt size))
        even-base (square (if (odd? base) (inc base) base))]
    (- (int (Math/sqrt even-base)) 2)))

(is (= 0 (start-pos-x 1)))
(is (= 0 (start-pos-x 4)))
(is (= 2 (start-pos-x 9)))
(is (= 2 (start-pos-x 16)))

(defn start-pos [size]
  (let [x (start-pos-x size)
        y (int (/ (shape-dim size) 2))]
    [x y]))

(is (= [0 0] (start-pos 1)))
(is (= [0 1] (start-pos 4)))
(is (= [2 3] (start-pos 16)))

(defn next-pos [[x y] [move-x move-y]]
  [(+ x move-x) (+ y move-y)])

(defn can-turn? [pos move shape]
  (= \space (get-in shape (next-pos pos move))))

(is (= true (can-turn? [0 0] [1 1] ["1 "
                                    "  "])))

(is (= false (can-turn? [0 0] [1 1] ["1 "
                                     " 2"])))

(defn next-turn [current-direction]
  (condp = current-direction
    [ 1  1] [ 1 -1]
    [ 1 -1] [-1 -1]
    [-1 -1] [-1  1]
    [-1  1] [ 1  1]
    [ 0  0] [ 1  1]))

(defn place [{:keys [shape pos forward turn]} elem]
  (if (can-turn? pos turn shape)
    (let [pos-after-move (next-pos pos turn)]
      {:shape (assoc-in shape pos-after-move elem)
       :pos  pos-after-move
       :forward turn
       :turn (next-turn turn)})
    (let [pos-after-move (next-pos pos forward)]
      {:shape (assoc-in shape pos-after-move elem)
       :pos  pos-after-move
       :forward forward
       :turn turn})))

(is (= [[\space \1 \space]
        [\space \space \2]
        [\space \space \space]]
       (:shape (place {:shape [[\space \1 \space]
                               [\space \space \space]
                               [\space \space \space]]
                       :pos [0 1]
                       :forward [1 1]
                       :turn [1 1]}
                      \2))))

(is (= [[\space \1 \space]
        [\space \space \2]
        [\space \3 \space]]
       (:shape (place {:shape [[\space \1 \space]
                               [\space \space \2]
                               [\space \space \space]]
                       :pos [1 2]
                       :forward [1 1]
                       :turn [1 -1]}
                      \3))))

(defn make-shape [start end]
  (let [elems (pad (digits (squares start end)))
        init-shape (empty-shape (shape-dim (count elems)))
        init-pos (start-pos (count elems))
        init-state {:shape init-shape :pos init-pos :forward [0 0] :turn [0 0]}
        final-state (reduce place init-state elems)
        final-shape (:shape final-state)
        vector-of-strings (mapv #(apply str %) final-shape)]
    vector-of-strings))

(defn make-shape-4clojure [start end]
  (letfn [(square [x]
            (* x x))
          (squares [start end]
            (take-while #(<= % end) (iterate square start)))
          (digits [numbers]
            (vec (mapcat str numbers)))
          (shape-dim [num-elems]
            (->> num-elems
                 (Math/sqrt)
                 (Math/round)
                 (* 2)
                 dec))
          (pad [ds]
            (let [size (count ds)
                  upper-limit (->> size
                                   (Math/sqrt)
                                   (Math/ceil)
                                   (Math/round)
                                   (square))]
              (vec (take upper-limit (concat ds (repeat \*))))))
          (empty-shape [dim]
            (vec (repeat dim (vec (repeat dim \ )))))
          (start-pos-x [size]
            (let [base (int (Math/sqrt size))
                  even-base (square (if (odd? base) (inc base) base))]
              (- (int (Math/sqrt even-base)) 2)))
          (start-pos [size]
            (let [x (start-pos-x size)
                  y (int (/ (shape-dim size) 2))]
              [x y]))
          (next-pos [[x y] [move-x move-y]]
            [(+ x move-x) (+ y move-y)])
          (can-turn? [pos move shape]
            (= \space (get-in shape (next-pos pos move))))
          (next-turn [current-direction]
            (condp = current-direction
              [ 1  1] [ 1 -1]
              [ 1 -1] [-1 -1]
              [-1 -1] [-1  1]
              [-1  1] [ 1  1]
              [ 0  0] [ 1  1]))
          (place [{:keys [shape pos forward turn]} elem]
            (if (can-turn? pos turn shape)
              (let [pos-after-move (next-pos pos turn)]
                {:shape (assoc-in shape pos-after-move elem)
                 :pos  pos-after-move
                 :forward turn
                 :turn (next-turn turn)})
              (let [pos-after-move (next-pos pos forward)]
                {:shape (assoc-in shape pos-after-move elem)
                 :pos  pos-after-move
                 :forward forward
                 :turn turn})))]
    (let [elems (pad (digits (squares start end)))
          init-shape (empty-shape (shape-dim (count elems)))
          init-pos (start-pos (count elems))
          init-state {:shape init-shape :pos init-pos :forward [0 0] :turn [0 0]}
          final-state (reduce place init-state elems)
          final-shape (:shape final-state)
          vector-of-strings (mapv #(apply str %) final-shape)]
      vector-of-strings)))


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
