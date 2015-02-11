(use '[clojure.test :only (is)])

(defn f2 [xs]
  (->> (mapcat (fn [x] [x x]) xs)
       (apply array-map)
       (vals)))

(defn f [xs]
  (first (reduce (fn [[ret set] x]
                   (if (contains? set x)
                     [ret set]
                     [(conj ret x) (conj set x)]))
                 [[] #{}]
                 xs)))

(is (= (f [1 2 1 3 1 2 4]) [1 2 3 4]))
(is (= (f [:a :a :b :b :c :c]) [:a :b :c]))
(is (= (f '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3])))
(is (= (f (range 50)) (range 50)))
