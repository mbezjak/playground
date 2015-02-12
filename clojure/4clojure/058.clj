(use '[clojure.test :only (is)])

(defn f2 [& fns]
  (fn [& first-args]
    (first (reduce (fn [args f]
                     [(apply f args)])
                   first-args
                   (reverse fns)))))

(defn f [first-fn & fns]
  (fn [& args]
    (if (empty? fns)
      (apply first-fn args)
      (first-fn (apply (apply f fns) args)))))

(is (= [3 2 1] ((f rest reverse) [1 2 3 4])))
(is (= 5 ((f (partial + 3) second) [1 2 3 4])))
(is (= true ((f zero? #(mod % 8) +) 3 5 7 9)))
(is (= "HELLO" ((f #(.toUpperCase %) #(apply str %) take) 5 "hello world")))
