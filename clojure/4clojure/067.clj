(use '[clojure.test :only (is)])

(defn f [x]
  (let [prime? (fn [n]
                 (let [upper-bound (inc (int (Math/sqrt n)))]
                   (every? (fn [y] (not= (mod n y) 0)) (range 2 upper-bound))))]
    (take x (filter prime? (iterate inc 2)))))

(is (= (f 2) [2 3]))
(is (= (f 5) [2 3 5 7 11]))
(is (= (last (f 100)) 541))
