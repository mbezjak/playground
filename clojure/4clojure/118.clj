(defn map1 [f xs]
  (if (empty? xs)
    xs
    (concat (list (f (first xs)))
            (lazy-seq (map1 f (rest xs))))))

(defn map2 [f xs]
  (if (empty? xs)
    xs
    (let [[fst & rst] xs]
      (cons (f fst) (lazy-seq (map2 f rst))))))

(assert (= [3 4 5 6 7]
           (map2 inc [2 3 4 5 6])))

(assert (= (repeat 10 nil)
           (map2 (fn [_] nil) (range 10))))

(assert (= [1000000 1000001]
           (->> (map2 inc (range))
                (drop (dec 1000000))
                (take 2))))
