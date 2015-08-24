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

ee
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
