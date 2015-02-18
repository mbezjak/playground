(use '[clojure.test :only (is)])

(defn f [xs]
  (let [e1 {:x-black-box "unique element"}
        e2 {:x-black-box "another unique"}
        ys (conj xs e1)
        zs (conj ys e2)]
    (cond
      (= zs (conj xs e2)) :map
      (= zs (conj zs e2)) :set
      (= (list e2 e1) (take 2 zs)) :list
      :else :vector)))

(is (= :map (f {:a 1, :b 2})))
(is (= :list (f (range (rand-int 20)))))
(is (= :vector (f [1 2 3 4 5 6])))
(is (= :set (f #{10 (rand-int 5)})))
(is (= [:map :set :vector :list] (map f [{} #{} [] ()])))
