(defn f [xs]
  (let [bs (reverse (map #(read-string (.toString %)) xs))
        powers (iterate #(* 2 %) 1)]
    (apply + (map * bs powers))))

(defn f2 [xs]
  (let [to-num #(read-string (.toString %))
        power #(int (Math/pow 2 %))
        raise (fn [idx char] (* (power idx) (to-num char)))]
    (apply + (map-indexed raise (reverse xs)))))

(assert (= 0     (f2 "0")))
(assert (= 7     (f2 "111")))
(assert (= 8     (f2 "1000")))
(assert (= 9     (f2 "1001")))
(assert (= 255   (f2 "11111111")))
(assert (= 1365  (f2 "10101010101")))
(assert (= 65535 (f2 "1111111111111111")))
