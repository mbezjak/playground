(use '[clojure.test :only (is)])

(defn f [s]
  (let [square (fn [n] (* n n))
        perfect-square? (fn [n] (= n (square (int (Math/sqrt n)))))]
    (->> (.split s ",")
         (map read-string)
         (filter perfect-square?)
         (clojure.string/join ","))))

(is (= (f "4,5,6,7,8,9") "4,9"))
(is (= (f "15,16,25,36,37") "16,25,36"))
