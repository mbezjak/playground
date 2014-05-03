(defn f [v xs]
  (butlast (mapcat #(list % v) xs)))

(assert (= (f 0 [1 2 3]) [1 0 2 0 3]))
(assert (= (apply str (f ", " ["one" "two" "three"])) "one, two, three"))
(assert (= (f :z [:a :b :c :d]) [:a :z :b :z :c :z :d]))
