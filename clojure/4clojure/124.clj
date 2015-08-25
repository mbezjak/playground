(use '[clojure.test :only (is)])

(defn valid? [[color & forward-move]]
  (cond
    (empty? forward-move) false
    (= 'e (first forward-move)) false
    (= color (first forward-move)) false
    (= 'e (last forward-move)) true
    :else false))

(is (= false (valid? '[w])))
(is (= false (valid? '[w e])))
(is (= false (valid? '[w e e])))
(is (= false (valid? '[w b])))
(is (= false (valid? '[w b b])))
(is (= false (valid? '[w w])))
(is (= false (valid? '[w w w])))
(is (= false (valid? '[w b w])))
(is (= false (valid? '[w w b])))
(is (= false (valid? '[w w e])))
(is (= true  (valid? '[w b e])))
(is (= true  (valid? '[w b b e])))

(defn truncate [move]
  (->> move
       (reverse)
       (drop-while #(= % 'e))
       (cons 'e)
       (reverse)
       (vec)))

(is (= '[w b e] (truncate '[w b e e])))
(is (= '[w b e] (truncate '[w b e])))

(defn color-positions [color board]
  (set
   (for [[ridx row] (map-indexed vector board)
         [cidx cell] (map-indexed vector row)
         :when (= cell color)]
     [ridx cidx])))

(is (= #{[1 1] [2 2]}
       (color-positions 'w '[[e e e e]
                             [e w b e]
                             [e b w e]
                             [e e e e]])))

(defn in-bounds? [[ridx cidx]]
  (and (>= ridx 0)
       (>= cidx 0)
       (< ridx 4)
       (< cidx 4)))

(defn diagonal-down-move [start-pos board]
  (let [up-left-pos (last (take-while in-bounds? (iterate #(mapv dec %) start-pos)))]
    (vec (for [i (range 4)
               :let [pos (mapv #(+ % i) up-left-pos)]
               :when (in-bounds? pos)]
           (get-in board pos)))))

(is (= '[e w e]
       (diagonal-down-move [2 1] '[[e e e e]
                                   [e e b e]
                                   [e w w e]
                                   [e e e e]])))

(defn diagonal-up-move [start-pos board]
  (let [[down-row down-col] (last (take-while in-bounds? (iterate (fn [[r c]] [(inc r) (dec c)]) start-pos)))]
    (vec (for [i (range 4)
               :let [pos [(- down-row i) (+ down-col i)]]
               :when (in-bounds? pos)]
           (get-in board pos)))))

(is (= '[e w b e]
       (diagonal-up-move [2 1] '[[e e e e]
                                 [e e b e]
                                 [e w w e]
                                 [e e e e]])))

(defn all-moves-from [[ridx cidx :as pos] board]
  (let [size (count board)
        row-level (get board ridx)
        col-level (mapv #(get % cidx) board)
        diag-down (diagonal-down-move pos board)
        diag-up (diagonal-up-move pos board)]

    (set [row-level col-level diag-down diag-up])))

(is (= '#{[e w w e]
          [e e w e]
          [e w e]
          [e w b e]}
       (all-moves-from [2 1] '[[e e e e]
                               [e e b e]
                               [e w w e]
                               [e e e e]])))

(defn analyze [board color]
  )

(is (= {[1 3] #{[1 2]}, [0 2] #{[1 2]}, [3 1] #{[2 1]}, [2 0] #{[2 1]}}
       (analyze '[[e e e e]
                  [e w b e]
                  [e b w e]
                  [e e e e]] 'w)))

(is (= {[3 2] #{[2 2]}, [3 0] #{[2 1]}, [1 0] #{[1 1]}}
       (analyze '[[e e e e]
                  [e w b e]
                  [w w w e]
                  [e e e e]] 'b)))

(is (= {[0 3] #{[1 2]}, [1 3] #{[1 2]}, [3 3] #{[2 2]}, [2 3] #{[2 2]}}
       (analyze '[[e e e e]
                  [e w b e]
                  [w w b e]
                  [e e b e]] 'w)))

(is (= {[0 3] #{[2 1] [1 2]}, [1 3] #{[1 2]}, [2 3] #{[2 1] [2 2]}}
       (analyze '[[e e w e]
                  [b b w e]
                  [b w w e]
                  [b w w w]] 'b)))
