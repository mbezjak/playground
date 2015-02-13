(use '[clojure.test :only (is)])

(defn f [n xs]
  (if (< (count xs) n)
    nil
    (let [[part rest] (split-at n xs)]
      (cons part (f n rest)))))

(is (= (f 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8))))
(is (= (f 2 (range 8)) '((0 1) (2 3) (4 5) (6 7))))
(is (= (f 3 (range 8)) '((0 1 2) (3 4 5))))
