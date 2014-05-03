(defn f [xs n]
  (->> xs
       (map vector (map inc (range)))
       (filter #(not (zero? (mod (first %) n))))
       (map second)))

(defn f2 [xs n]
  (->> xs
       (map vector (cycle (concat (repeat (dec n) true) '(false))))
       (filter #(true? (first %)))
       (map second)))

(defn f3 [xs n]
  (letfn [(inner [ys c]
            (cond
             (empty? ys) ys
             (= 1 c) (inner (rest ys) n)
             :else (conj (inner (rest ys) (dec c)) (first ys))))]
    (inner xs n)))

(defn f4 [xs n]
  (let [[left right] (split-at (dec n) xs)]
    (if (empty? right)
      left
      (concat left (f4 (rest right) n)))))

(defn f5 [xs n]
  (apply concat (partition (dec n) n [] xs)))

(assert (= (f [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
(assert (= (f [:a :b :c :d :e :f] 2) [:a :c :e]))
(assert (= (f [1 2 3 4 5 6] 4) [1 2 3 5 6]))
