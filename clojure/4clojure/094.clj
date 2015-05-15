(use '[clojure.test :only (is)])

(def sample-board
  ["      "
   " ##   "
   " ##   "
   "   ## "
   "   ## "
   "      "])

(defn map-board [board f]
  (vec (map-indexed (fn [ridx row]
                      (apply str (map-indexed (fn [cidx cell]
                                                (f cell ridx cidx))
                                              row)))
                    board)))

(is (= (map-board sample-board (fn [cell ridx cidx] "x"))
       ["xxxxxx"
        "xxxxxx"
        "xxxxxx"
        "xxxxxx"
        "xxxxxx"
        "xxxxxx"]))

(defn format-cell [kw]
  (if (= :alive kw) "#" " "))

(is (= "#" (format-cell :alive)))
(is (= " " (format-cell :dead)))

(defn alive? [cell]
  (= cell \#))

(is (= true  (alive? \#)))
(is (= false (alive? \ )))

(defn neighbors-of [row col]
  (set (for [nbor-row (range (dec row) (+ 2 row))
             nbor-col (range (dec col) (+ 2 col))
             :when (not (and (= nbor-row row) (= nbor-col col)))]
         [nbor-row nbor-col])))

(is (= (neighbors-of 3 3)
       #{[2 2] [2 3] [2 4]
         [3 2]       [3 4]
         [4 2] [4 3] [4 4]}))

(defn in-board? [dim [row col]]
  (and
   (>= row 0)
   (>= col 0)
   (< row dim)
   (< col dim)))

(is (= true  (in-board? 3 [ 0  0])))
(is (= true  (in-board? 3 [ 1  1])))
(is (= true  (in-board? 3 [ 2  2])))
(is (= false (in-board? 3 [-1  1])))
(is (= false (in-board? 3 [ 1 -1])))
(is (= false (in-board? 3 [ 3  1])))
(is (= false (in-board? 3 [ 1  4])))
(is (= false (in-board? 3 [ 4  1])))
(is (= false (in-board? 3 [ 1  4])))

(defn valid-neighbors-of [dim row col]
  (set (filter (partial in-board? dim) (neighbors-of row col))))

(is (= (valid-neighbors-of 6 0 0)
       #{      [0 1]
         [1 0] [1 1]}))

(defn count-alive-neighbors-of [board row col]
  (count (filter alive? (for [[nbor-row nbor-col] (valid-neighbors-of (count board) row col)]
                          (get-in board [nbor-row nbor-col])))))

(is (= 3 (count-alive-neighbors-of sample-board 1 1)))

(defn cell-next-generation [board cell row col]
  (let [alive-cell (alive? cell)
        alive-nbor-count (count-alive-neighbors-of board row col)]
    (cond
      (and alive-cell (< alive-nbor-count 2)) :dead
      (and alive-cell (contains? #{2 3} alive-nbor-count)) :alive
      (and alive-cell (> alive-nbor-count 3)) :dead
      (and (not alive-cell) (= 3 alive-nbor-count)) :alive
      :else :dead)))

(is (= :alive (cell-next-generation sample-board \# 1 1)))
(is (= :dead (cell-next-generation sample-board \# 2 2)))

(defn next-generation [board]
  (map-board board
             (fn [cell row col]
               (format-cell (cell-next-generation board cell row col)))))

(defn next-generation2 [board]
  (letfn [(map-board [f]
            (vec (map-indexed (fn [ridx row]
                                (apply str (map-indexed (fn [cidx cell]
                                                          (f cell ridx cidx))
                                                        row)))
                              board)))
          (format-cell [kw]
            (if (= :alive kw) "#" " "))
          (alive? [cell]
            (= cell \#))
          (neighbors-of [row col]
            (set (for [nbor-row (range (dec row) (+ 2 row))
                       nbor-col (range (dec col) (+ 2 col))
                       :when (not (and (= nbor-row row) (= nbor-col col)))]
                   [nbor-row nbor-col])))
          (in-board? [dim [row col]]
            (and
             (>= row 0)
             (>= col 0)
             (< row dim)
             (< col dim)))
          (valid-neighbors-of [dim row col]
            (set (filter (partial in-board? dim) (neighbors-of row col))))
          (count-alive-neighbors-of [row col]
            (count (filter alive? (for [[nbor-row nbor-col] (valid-neighbors-of (count board) row col)]
                                    (get-in board [nbor-row nbor-col])))))
          (cell-next-generation [cell row col]
            (let [alive-cell (alive? cell)
                  alive-nbor-count (count-alive-neighbors-of row col)]
              (cond
                (and alive-cell (< alive-nbor-count 2)) :dead
                (and alive-cell (contains? #{2 3} alive-nbor-count)) :alive
                (and alive-cell (> alive-nbor-count 3)) :dead
                (and (not alive-cell) (= 3 alive-nbor-count)) :alive
                :else :dead)))]
    (map-board
     (fn [cell row col]
       (format-cell (cell-next-generation cell row col))))))


(is (= (next-generation ["      "
                         " ##   "
                         " ##   "
                         "   ## "
                         "   ## "
                         "      "])
       ["      "
        " ##   "
        " #    "
        "    # "
        "   ## "
        "      "]))

(is (= (next-generation ["     "
                         "     "
                         " ### "
                         "     "
                         "     "])
       ["     "
        "  #  "
        "  #  "
        "  #  "
        "     "]))

(is (= (next-generation ["      "
                         "      "
                         "  ### "
                         " ###  "
                         "      "
                         "      "])
       ["      "
        "   #  "
        " #  # "
        " #  # "
        "  #   "
        "      "]))
