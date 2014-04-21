(def x #{:a :b :c :d})

(assert (= x (set '(:a :a :b :c :c :c :c :d :d))))
(assert (= x (clojure.set/union #{:a :b :c} #{:b :c :d})))
