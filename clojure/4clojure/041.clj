(defn f [xs n]
  (->> xs
       (map vector (map inc (range)))
       (filter #(not (zero? (mod (first %) n))))
       (map second)))

(assert (= (f [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
(assert (= (f [:a :b :c :d :e :f] 2) [:a :c :e]))
(assert (= (f [1 2 3 4 5 6] 4) [1 2 3 5 6]))
