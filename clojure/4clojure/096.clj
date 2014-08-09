(defn mirror [tree]
  (if (nil? tree)
    nil
    (let [[v l r] tree]
      (list v (mirror r) (mirror l)))))

(defn symmetric? [tree]
  (if (nil? tree)
    true
    (let [[v l r] tree]
      (= (mirror l) r))))

(assert (= (symmetric? '(:a (:b nil nil) (:b nil nil))) true))
(assert (= (symmetric? '(:a (:b nil nil) nil)) false))
(assert (= (symmetric? '(:a (:b nil nil) (:c nil nil))) false))
(assert (= (symmetric? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                        [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
           true))
(assert (= (symmetric? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                        [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
           false))
(assert (= (symmetric? [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
                        [2 [3 nil [4 [6 nil nil] nil]] nil]])
           false))
