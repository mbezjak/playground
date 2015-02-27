(use '[clojure.test :only (is)])

(defn mwith [f & ms]
  (reduce (fn [acc m]
            (reduce (fn [acc [k v]]
                   (let [new-val (if (contains? acc k)
                                   (f (get acc k) v)
                                   v)]
                     (assoc acc k new-val)))
                    acc
                    m))
          {}
          ms))

(is (= (mwith * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
       {:a 4, :b 6, :c 20}))

(is (= (mwith - {1 10, 2 20} {1 3, 2 10, 3 15})
       {1 7, 2 10, 3 15}))

(is (= (mwith concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
       {:a [3 4 5], :b [6 7], :c [8 9]}))
