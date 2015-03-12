(use '[clojure.test :only (is)])

(defn lazy-search [& colls]
  (let [heads (map first colls)]
    (if (apply = heads)
      (first heads)
      (let [biggest (apply max heads)
            no-lt-biggest (map (fn [coll] (filter #(<= biggest %) coll)) colls)]
        (apply lazy-search no-lt-biggest)))))

(is (= 3 (lazy-search [3 4 5])))

(is (= 4 (lazy-search [1 2 3 4 5 6 7] [0.5 3/2 4 19])))

(is (= 7 (lazy-search (range) (range 0 100 7/6) [2 3 5 7 11 13])))

(is (= 64 (lazy-search (map #(* % % %) (range)) ;; perfect cubes
                       (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
                       (iterate inc 20)))) ;; at least as large as 20
