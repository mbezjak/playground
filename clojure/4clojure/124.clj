(use '[clojure.test :only (is)])
(require 'clojure.set)

(defn opposite-of [color]
  (if (= color 'w) 'b 'w))

(defn valid? [move]
  (let [move-str (clojure.string/join "" move)]
    (boolean (or (re-matches #"e*w+b+e+" move-str)
                 (re-matches #"e*b+w+e+" move-str)))))

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
(is (= false (valid? '[e w e e])))
(is (= true  (valid? '[w b e])))
(is (= true  (valid? '[e w b e])))
(is (= true  (valid? '[w b b e])))

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
           (list (get-in board pos) pos)))))

(is (= '[(e [1 0]) (w [2 1]) (e [3 2])]
       (diagonal-down-move [2 1] '[[e e e e]
                                   [e e b e]
                                   [e w w e]
                                   [e e e e]])))

(defn diagonal-up-move [start-pos board]
  (let [[down-row down-col] (last (take-while in-bounds? (iterate (fn [[r c]] [(inc r) (dec c)]) start-pos)))]
    (vec (for [i (range 4)
               :let [pos [(- down-row i) (+ down-col i)]]
               :when (in-bounds? pos)]
           (list (get-in board pos) pos)))))

(is (= '[(e [3 0]) (w [2 1]) (b [1 2]) (e [0 3])]
       (diagonal-up-move [2 1] '[[e e e e]
                                 [e e b e]
                                 [e w w e]
                                 [e e e e]])))

(defn all-moves-from [[ridx cidx :as pos] board]
  (let [size (count board)
        row-level (vec (map-indexed (fn [idx itm] (list itm [ridx idx])) (get board ridx)))
        col-level (vec (map-indexed (fn [idx itm] (list (get itm cidx) [idx cidx])) board))
        diag-down (diagonal-down-move pos board)
        diag-up (diagonal-up-move pos board)]

    (set [row-level col-level diag-down diag-up])))

(is (= '#{[(e [2 0]) (w [2 1]) (w [2 2]) (e [2 3])]
          [(e [0 1]) (e [1 1]) (w [2 1]) (e [3 1])]
          [(e [1 0]) (w [2 1]) (e [3 2])]
          [(e [3 0]) (w [2 1]) (b [1 2]) (e [0 3])]}
       (all-moves-from [2 1] '[[e e e e]
                               [e e b e]
                               [e w w e]
                               [e e e e]])))

(defn valid-moves [pos board]
  (set (for [move (all-moves-from pos board)
             :let [color-move (map first move)]
             :when (valid? color-move)]
         move)))

(is (= '#{[(e [1 0]) (w [1 1]) (b [1 2]) (e [1 3])]}
       (valid-moves [1 1] '[[e e e e]
                            [e w b e]
                            [e e e e]
                            [e e e e]])))

(is (= '#{[(e [1 0]) (w [1 1]) (b [1 2]) (e [1 3])]
          [(e [0 1]) (w [1 1]) (b [2 1]) (e [3 1])]}
       (valid-moves [1 1] '[[e e e e]
                            [e w b e]
                            [e b w e]
                            [e e e e]])))

(defn positions-of-flips [move color]
  (->> move
       (filter (comp #(and (not= % 'e) (not= % color)) first))
       (map second)
       (set)))

(is (= #{[1 2]}
       (positions-of-flips '[(e [3 0]) (w [2 1]) (b [1 2]) (e [0 3])] 'w)))

(defn jump-position [move color]
  (let [move-str (clojure.string/join "" (map first move))
        pattern-fwd (re-pattern (str "e*" color "+" (opposite-of color) "+e+"))
        pattern-bck (re-pattern (str "e*" (opposite-of color) "+" color "+e+"))]
    (if (re-matches pattern-fwd move-str)
      (last (last move))
      (last (first move)))))

(is (= [0 3]
       (jump-position '[(e [3 0]) (w [2 1]) (b [1 2]) (e [0 3])] 'w)))

(is (= [3 0]
       (jump-position '[(e [3 0]) (b [2 1]) (w [1 2]) (e [0 3])] 'w)))

(defn analyze [board color]
  (apply merge-with clojure.set/union
         (for [color-pos (color-positions color board)
               move (valid-moves color-pos board)]
           {(jump-position move color) (positions-of-flips move color)})))

(defn analyze-4clojure [board color]
  (letfn [(opposite-of [color]
            (if (= color 'w) 'b 'w))
          (valid? [move]
            (let [move-str (clojure.string/join "" move)]
              (boolean (or (re-matches #"e*w+b+e+" move-str)
                           (re-matches #"e*b+w+e+" move-str)))))
          (color-positions [color board]
            (set
             (for [[ridx row] (map-indexed vector board)
                   [cidx cell] (map-indexed vector row)
                   :when (= cell color)]
               [ridx cidx])))
          (in-bounds? [[ridx cidx]]
            (and (>= ridx 0)
                 (>= cidx 0)
                 (< ridx 4)
                 (< cidx 4)))
          (diagonal-down-move [start-pos board]
            (let [up-left-pos (last (take-while in-bounds? (iterate #(mapv dec %) start-pos)))]
              (vec (for [i (range 4)
                         :let [pos (mapv #(+ % i) up-left-pos)]
                         :when (in-bounds? pos)]
                     (list (get-in board pos) pos)))))
          (diagonal-up-move [start-pos board]
            (let [[down-row down-col] (last (take-while in-bounds? (iterate (fn [[r c]] [(inc r) (dec c)]) start-pos)))]
              (vec (for [i (range 4)
                         :let [pos [(- down-row i) (+ down-col i)]]
                         :when (in-bounds? pos)]
                     (list (get-in board pos) pos)))))
          (all-moves-from [[ridx cidx :as pos] board]
            (let [size (count board)
                  row-level (vec (map-indexed (fn [idx itm] (list itm [ridx idx])) (get board ridx)))
                  col-level (vec (map-indexed (fn [idx itm] (list (get itm cidx) [idx cidx])) board))
                  diag-down (diagonal-down-move pos board)
                  diag-up (diagonal-up-move pos board)]

              (set [row-level col-level diag-down diag-up])))
          (valid-moves [pos board]
            (set (for [move (all-moves-from pos board)
                       :let [color-move (map first move)]
                       :when (valid? color-move)]
                   move)))
          (positions-of-flips [move color]
            (->> move
                 (filter (comp #(and (not= % 'e) (not= % color)) first))
                 (map second)
                 (set)))
          (jump-position [move color]
            (let [move-str (clojure.string/join "" (map first move))
                  pattern-fwd (re-pattern (str "e*" color "+" (opposite-of color) "+e+"))
                  pattern-bck (re-pattern (str "e*" (opposite-of color) "+" color "+e+"))]
              (if (re-matches pattern-fwd move-str)
                (last (last move))
                (last (first move)))))]

    (apply merge-with clojure.set/union
           (for [color-pos (color-positions color board)
                 move (valid-moves color-pos board)]
             {(jump-position move color) (positions-of-flips move color)}))))

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
