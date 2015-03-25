(use '[clojure.test :only (is)])

(defn engine2 [formula]
  (fn [vars]
    (let [[op & args] formula
          extract-num (fn [x]
                        (cond
                          (symbol? x) (get vars x)
                          (sequential? x) ((engine2 x) vars)
                          :else x))
          nums (map extract-num args)
          op-fn (cond
                  (= op '+) +
                  (= op '-) -
                  (= op '*) *
                  (= op '/) /)]
      (apply op-fn nums))))

(defn engine [formula]
  (fn [vars]
    (let [[op & args] formula
          extract-num (fn [x]
                        (cond
                          (symbol? x) (get vars x)
                          (sequential? x) ((engine x) vars)
                          :else x))
          nums (map extract-num args)]
      (apply @(resolve op) nums))))


(is (= 2 ((engine '(/ a b))
          '{b 8 a 16})))

(is (= 8 ((engine '(+ a b 2))
          '{a 2 b 4})))

(is (= [6 0 -4]
       (map (engine '(* (+ 2 a)
                    (- 10 b)))
            '[{a 1 b 8}
              {b 5 a -2}
              {a 2 b 11}])))

(is (= 1 ((engine '(/ (+ x 2)
                  (* 3 (+ y 1))))
          '{x 4 y 1})))
