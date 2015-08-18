(use '[clojure.test :only (is)])
(require 'clojure.string)


(defn f [& ints]
  (reify
    clojure.lang.Seqable (seq [this] (not-empty (distinct ints)))
    Object (toString [this] (clojure.string/join ", " (sort ints)))))


(is (= "1, 2, 3" (str (f 2 1 3))))
(is (= '(2 1 3) (seq (f 2 1 3))))
(is (= '(2 1 3) (seq (f 2 1 3 3 1 2))))
(is (= '(1) (seq (apply f (repeat 5 1)))))
(is (= "1, 1, 1, 1, 1" (str (apply f (repeat 5 1)))))
(is (and (= nil (seq (f)))
         (=  "" (str (f)))))

(is (= nil (seq (f))))
(is (= "" (str (f))))
