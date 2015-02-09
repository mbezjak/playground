(defn f [xs n]
  (let [idx-itm (map-indexed (fn [idx itm] [idx itm]) xs)
        groups (group-by (fn [[idx itm]] (rem idx n)) idx-itm)
        subsequences (map #(map second %) (vals groups))]
    subsequences))

(assert (= (f [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6))))
(assert (= (f (range 9) 3) '((0 3 6) (1 4 7) (2 5 8))))
(assert (= (f (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9))))
