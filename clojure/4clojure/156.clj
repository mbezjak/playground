(defn f [defval keys]
  (->> keys (mapcat (fn [k] [k defval])) (apply hash-map)))

(assert (= (f 0 [:a :b :c]) {:a 0 :b 0 :c 0}))
(assert (= (f "x" [1 2 3]) {1 "x" 2 "x" 3 "x"}))
(assert (= (f [:a :b] [:foo :bar]) {:foo [:a :b] :bar [:a :b]}))
