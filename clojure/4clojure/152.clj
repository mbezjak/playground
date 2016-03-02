(use '[clojure.test :only (is)])

(defn latin-square? [vectors]
  (let [size (count vectors)]
    (and
     (= (count (distinct (flatten vectors))) size)
     (every? #(= (count (distinct %)) size) vectors)
     (every? #(= (count (distinct %)) size) (apply map vector vectors)))))

(is (latin-square? [[1 2]
                    [2 1]]))

(is (latin-square? '[[A B C D E F]
                     [B C D E F A]
                     [C D E F A B]
                     [D E F A B C]
                     [E F A B C D]
                     [F A B C D E]]))

(is (not (latin-square? [[1]
                         [2 1]])))

(is (not (latin-square? [[1 2]
                         [1 2]])))

(is (not (latin-square? [[1 1]
                         [2 2]])))

(is (not (latin-square? [[1 3]
                         [2 1]])))

(defn order [latin-square]
  (count latin-square))

(defn pad-vectors [vectors]
  (let [max-row-size (apply max (map count vectors))]
    (mapv #(vec (concat % (repeat (- max-row-size (count %)) :e))) vectors)))

(defn rearrangements [vector]
  (let [num-floaters (count (filter #{:e} vector))
        rigid-part (filter #(not= % :e) vector)]
    (if (zero? num-floaters)
      [vector]
      (vec (for [before (range (inc num-floaters))
                 :let [after (- num-floaters before)]]
             (vec (concat
                   (repeat before :e)
                   rigid-part
                   (repeat after :e))))))))

(is (= (rearrangements [1 2])
       [[1 2]]))

(is (= (rearrangements [1 2 :e])
       [[1 2 :e]
        [:e 1 2]]))

(defn alignments [vectors]
  (if (empty? vectors)
    [[]]
    (vec
     (for [other (alignments (vec (rest vectors)))
           arrangement (rearrangements (first vectors))]
       (vec (concat [arrangement] other))))))

(is (= 1 (count (alignments '[[A B C D]
                              [A C D B]
                              [B A D C]
                              [D C A B]]))))

(is (= 2 (count (alignments [[2 4 6 3]
                             [3 4 6 2]
                             [6 2 4 :e]]))))

(defn valid-square? [square]
  (every? #(not= % :e) (flatten square)))

(defn squares [alignment]
  (for [[ridx row] (map-indexed vector alignment)
        [cidx col] (map-indexed vector row)
        size (range 2 (inc (min
                            (- (count alignment) ridx)
                            (- (count row) cidx))))
        :let [potential-square (mapv #(subvec % cidx (+ cidx size)) (subvec alignment ridx (+ ridx size)))]
        :while (valid-square? potential-square)]
    potential-square))

(is (= (squares [[1 2 :e]
                 [3 4 :e]])
       [[[1 2]
         [3 4]]]))

(is (= (squares [[1 2 :e]
                 [3 4 :e]
                 [5 6 :e]])
       [[[1 2]
         [3 4]]
        [[3 4]
         [5 6]]]))

(is (= (squares [[1 2]
                 [3 4]])
       [[[1 2]
         [3 4]]]))

(is (= (squares [[1 2 3]
                 [4 5 6]
                 [7 8 9]])
       [[[1 2]
         [4 5]]
        [[1 2 3]
         [4 5 6]
         [7 8 9]]
        [[2 3]
         [5 6]]
        [[4 5]
         [7 8]]
        [[5 6]
         [8 9]]]))

(defn latin-squares [vectors]
  (for [alignment (alignments (pad-vectors vectors))
        square (squares alignment)
        :when (latin-square? square)]
    square))

(defn latin-square-orders [vectors]
  (->> vectors
       (latin-squares)
       (distinct)
       (map order)
       (frequencies)))

(is (= (latin-square-orders '[[A B C D]
                              [A C D B]
                              [B A D C]
                              [D C A B]])
       {}))

(is (= (latin-square-orders '[[A B C D E F]
                              [B C D E F A]
                              [C D E F A B]
                              [D E F A B C]
                              [E F A B C D]
                              [F A B C D E]])
       {6 1}))

(is (= (latin-square-orders '[[A B C D]
                              [B A D C]
                              [D C B A]
                              [C D A B]])
       {4 1, 2 4}))

(is (= (latin-square-orders '[[B D A C B]
                              [D A B C A]
                              [A B C A B]
                              [B C A B C]
                              [A D B C A]])
       {3 3}))

(is (= (latin-square-orders [  [2 4 6 3]
                             [3 4 6 2]
                             [6 2 4]  ])
       {}))

(is (= (latin-square-orders [[1]
                             [1 2 1 2]
                             [2 1 2 1]
                             [1 2 1 2]
                             []       ])
       {2 2}))

(is (= (latin-square-orders [[3 1 2]
                             [1 2 3 1 3 4]
                             [2 3 1 3]    ])
       {3 1, 2 2}))

(is (= (latin-square-orders [[8 6 7 3 2 5 1 4]
                             [6 8 3 7]
                             [7 3 8 6]
                             [3 7 6 8 1 4 5 2]
                             [1 8 5 2 4]
                             [8 1 2 4 5]])
       {4 1, 3 1, 2 7}))
