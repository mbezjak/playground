(ns atom.core)

(defn memoize' [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

(defn fib [n]
  (if (<= n 1)
    n
    (+ (fib (dec n)) (fib (- n 2)))))

(defn testfib []
  (let [mfib (memoize' fib)]
    (time (mfib 33))
    (time (mfib 34))))
