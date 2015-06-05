(use '[clojure.test :only (is)])

(defn lines [board]
  (set (concat board
               (for [i (range 3)] (map #(nth % i) board))
               (vector (for [i (range 3)] (get-in board [i i])))
               (vector (for [i (range 3)] (get-in board [i (- 3 1 i)]))))))

(is (= (lines [[:o :e :e]
               [:o :x :o]
               [:x :x :e]])
       #{[:o :e :e]
         [:o :x :o]
         [:x :x :e]
         [:o :o :x]
         [:e :x :x]
         [:e :o :e]
         [:o :x :e]}))

(defn winner? [player lines]
  (or (some #(every? #{player} %) lines) false))

(is (= true  (winner? :x #{[:o :x :x] [:x :x :x]})))
(is (= false (winner? :x #{[:o :x :x] [:x :x :e]})))

(defn empty-positions [board]
  (set
   (for [[ridx row] (map-indexed vector board)
         [cidx itm] (map-indexed vector row)
         :when (= itm :e)]
     [ridx cidx])))

(is (= (empty-positions [[:o :e :e]
                         [:o :x :o]
                         [:x :x :e]])
       #{[0 1] [0 2] [2 2]}))

(defn win-position? [board position player]
  (winner? player (lines (assoc-in board position player))))

(defn win-moves [piece board]
  (set (filter #(win-position? board % piece)
               (empty-positions board))))

(defn win-moves-4clojure [player board]
  (letfn [(lines [board]
            (set (concat board
                         (for [i (range 3)] (map #(nth % i) board))
                         (vector (for [i (range 3)] (get-in board [i i])))
                         (vector (for [i (range 3)] (get-in board [i (- 3 1 i)]))))))
          (winner? [lines]
            (or (some #(every? #{player} %) lines) false))
          (empty-positions []
            (set
             (for [[ridx row] (map-indexed vector board)
                   [cidx itm] (map-indexed vector row)
                   :when (= itm :e)]
               [ridx cidx])))
          (win-position? [position]
            (winner? (lines (assoc-in board position player))))]

    (set (filter #(win-position? %) (empty-positions)))))


(is (= (win-moves :x [[:o :e :e]
                      [:o :x :o]
                      [:x :x :e]])
       #{[2 2] [0 1] [0 2]}))

(is (= (win-moves :x [[:x :o :o]
                      [:x :x :e]
                      [:e :o :e]])
       #{[2 2] [1 2] [2 0]}))

(is (= (win-moves :x [[:x :e :x]
                      [:o :x :o]
                      [:e :o :e]])
       #{[2 2] [0 1] [2 0]}))

(is (= (win-moves :x [[:x :x :o]
                      [:e :e :e]
                      [:e :e :e]])
       #{}))

(is (= (win-moves :o [[:x :x :o]
                      [:o :e :o]
                      [:x :e :e]])
       #{[2 2] [1 1]}))
