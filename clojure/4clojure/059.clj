(use '[clojure.test :only (is)])

(defn f [& fns]
  (fn [& args]
    (map #(apply % args) fns)))

(is (= [21 6 1] ((f + max min) 2 3 5 1 6 4)))
(is (= ["HELLO" 5] ((f #(.toUpperCase %) count) "hello")))
(is (= [2 6 4] ((f :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10})))
