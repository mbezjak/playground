(defn lcm [& args]
  (let [impl (fn self [xs xms]
               (if (apply == xms)
                 (first xms)
                 (let [x (apply min xms)
                       index (.indexOf xms x)
                       xinxs (nth xs index)
                       xm1s (assoc xms index (+ x xinxs))]
                   (self xs xm1s))))]
    (impl (apply vector args) (apply vector args))))


(assert (== (lcm 4 4) 4))
(assert (== (lcm 2 3) 6))
(assert (== (lcm 5 3 7) 105))
(assert (== (lcm 1/3 2/5) 2))
(assert (== (lcm 3/4 1/6) 3/2))
(assert (== (lcm 7 5/7 2 3/5) 210))
