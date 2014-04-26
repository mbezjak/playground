(assert (= 3
           (let [[f xs] [+ (range 3)]] (apply f xs))
           (let [[[f xs] b] [[+ 1] 2]] (f xs b))
           (let [[f xs] [inc 2]] (f xs))))
