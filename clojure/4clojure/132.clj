(use '[clojure.test :only (is)])

(defn insert-between [p val coll]
  (if (empty? coll)
    nil
    (let [adjacent-pairs (map vector coll (drop 1 coll))]
      (cons (first coll) (mapcat (fn [[x y]]
                                   (if (p x y) [val y] [y]))
                                 adjacent-pairs)))))

(is (= '(1 :less 6 :less 7 4 3) (insert-between < :less [1 6 7 4 3])))

(is (= '(2) (insert-between > :more [2])))

(is (= [0 1 :x 2 :x 3 :x 4]  (insert-between #(and (pos? %) (< % %2)) :x (range 5))))

(is (empty? (insert-between > :more ())))

(is (= [0 1 :same 1 2 3 :same 5 8 13 :same 21]
       (take 12 (->> [0 1]
                     (iterate (fn [[a b]] [b (+ a b)]))
                     (map first)           ; fibonacci numbers
                     (insert-between (fn [a b] ; both even or both odd
                                       (= (mod a 2) (mod b 2)))
                                     :same)))))
