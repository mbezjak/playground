(use '[clojure.test :only (is)])

(defn to-map2 [coll]
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



(defn to-map [coll]
  (letfn
      [(assoc-empty [map kw]
         (assoc map kw []))
       (assoc-conj [map kw val]
         (assoc map kw (conj (get map kw) val)))

       ;; state transitions
       (kw->slurp [map [kw & rst]]
         (partial slurp-values (assoc-empty map kw) kw rst))
       (slurp->kw [map coll]
         (partial new-keyword map coll))
       (slurp->slurp [map kw [val & rst]]
         (partial slurp-values (assoc-conj map kw val) kw rst))

       ;; state machine
       (new-keyword [map coll]
         (if (empty? coll)
           map
           (kw->slurp map coll)))
       (slurp-values [map kw coll]
         (cond
           (empty? coll) map
           (keyword? (first coll)) (slurp->kw map coll)
           :else (slurp->slurp map kw coll)))]

    (trampoline new-keyword {} coll)))

(is (= {} (to-map [])))
(is (= {:a [1]} (to-map [:a 1])))
(is (= {:a [1], :b [2]} (to-map [:a 1, :b 2])))
(is (= {:a [1 2 3], :b [], :c [4]} (to-map [:a 1 2 3 :b :c 4])))
