(use '[clojure.test :only (is)])

(defn tramp [f & args]
  (letfn [(loop [g]
            (let [retval (g)]
              (if (fn? retval)
                (loop retval)
                retval)))]
    (loop (apply f args))))

(letfn
    [(foo [x y] #(bar (conj x y) y))
     (bar [x y] (if (> (last x) 10)
                  x
                  #(foo x (+ 2 y))))]

  (is (= [1 3 5 7 9 11]
         (trampoline foo [] 1)
         (tramp foo [] 1))))
