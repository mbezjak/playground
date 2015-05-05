(use '[clojure.test :only (is)])

(defn chain? [words]
  (letfn [(char-diff [[a b]] (if (= a b) 0 1))
          (word-diff-same-len [w1 w2]
            (let [zip (map vector w1 w2)
                  diffs (map char-diff zip)
                  sum (apply + diffs)]
              (<= sum 1)))
          (word-diff [w1 w2]
            (let [c1 (count w1)
                  c2 (count w2)]
              (cond
                (= c1 c2) (word-diff-same-len w1 w2)
                (= c1 (inc c2)) (some true? (for [i (range c1)
                                                  :let [left (.substring w1 0 i)
                                                        right (.substring w1 (inc i) c1)
                                                        new-word (str left right)]]
                                              (word-diff-same-len new-word w2)))
                (= c1 (dec c2)) (word-diff w2 w1)
                :else false)))
          (is-chain [xs]
            (every? true? (for [[w1 w2] (map vector xs (rest xs))]
                            (word-diff w1 w2))))
          (permutations [xs]
            (if (empty? xs)
              [[]]
              (let [c (count xs)]
                (for [i (range c)
                      :let [x (nth xs i)
                            rst (vec (concat (subvec xs 0 i) (subvec xs (inc i) c)))]
                      p (permutations rst)]
                  (cons x p)))))]

    (or (some is-chain (permutations (vec words))) false)))

(is (= true (chain? #{"hat" "coat" "dog" "cat" "oat" "cot" "hot" "hog"})))
(is (= false (chain? #{"cot" "hot" "bat" "fat"})))
(is (= false (chain? #{"to" "top" "stop" "tops" "toss"})))
(is (= true (chain? #{"spout" "do" "pot" "pout" "spot" "dot"})))
(is (= true (chain? #{"share" "hares" "shares" "hare" "are"})))
(is (= false (chain? #{"share" "hares" "hare" "are"})))
