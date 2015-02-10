(use '[clojure.test :only (is)])

(defn f [xs]
  (->> (group-by identity xs)
       (mapcat (fn [[key vals]] [key (count vals)]))
       (apply hash-map)))

(is (= (f [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1}))
(is (= (f [:b :a :b :a :b]) {:a 2, :b 3}))
(is (= (f '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2}))
