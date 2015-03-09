(use '[clojure.test :only (is)])

(defn to-map [coll]
  (letfn [(into-current-key [acc cur-key xs]
            (if (empty? xs)
              acc
              (let [fst (first xs)
                    rst (rest xs)]
                (if (keyword? fst)
                  (into-current-key (assoc acc fst []) fst rst)
                  (let [lst (get acc cur-key)
                        new-lst (conj lst fst)
                        new-acc (assoc acc cur-key new-lst)]
                    (into-current-key new-acc cur-key rst))))))]

    (into-current-key {}  (first coll) coll)))

(is (= {} (to-map [])))
(is (= {:a [1]} (to-map [:a 1])))
(is (= {:a [1], :b [2]} (to-map [:a 1, :b 2])))
(is (= {:a [1 2 3], :b [], :c [4]} (to-map [:a 1 2 3 :b :c 4])))
