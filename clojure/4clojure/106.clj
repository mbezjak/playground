(use '[clojure.test :only (is)])

(defn do-operations [num]
  (let [double (* 2 num)
        half (quot num 2)
        add2 (+ 2 num)]
    (if (even? num)
      [double half add2]
      [double add2])))

(is (= [2 3]   (do-operations 1)))
(is (= [4 1 4] (do-operations 2)))
(is (= [6 5]   (do-operations 3)))
(is (= [8 2 6] (do-operations 4)))

(defn find-matching-node [end nodes]
  (some #(when (= end (:num %)) %) nodes))

(is (= nil      (find-matching-node 3 [{:num 1} {:num 2}])))
(is (= {:num 3} (find-matching-node 3 [{:num 1} {:num 2} {:num 3}])))

(defn next-level-nodes [node]
  (map (fn [after-num]
         {:num after-num :depth (inc (:depth node))})
       (do-operations (:num node))))

(is (= (next-level-nodes {:num 3 :depth 1})
       [{:num 6 :depth 2} {:num 5 :depth 2}]))

(defn next-level [end nodes]
  (let [match (find-matching-node end nodes)]
    (if match
      (:depth match)
      (recur end (mapcat next-level-nodes nodes)))))

(is (= 1 (next-level 3 [{:num 3 :depth 1}])))
(is (= 2 (next-level 6 [{:num 3 :depth 1}])))

(defn solve [start end]
  (next-level end [{:num start :depth 1}]))

;; 4clojure solution
(defn solve2 [start end]
  (letfn [(do-operations [num]
            (let [double (* 2 num)
                  half (quot num 2)
                  add2 (+ 2 num)]
              (if (even? num)
                [double half add2]
                [double add2])))
          (find-matching-node [nodes]
            (some #(when (= end (:num %)) %) nodes))
          (next-level-nodes [node]
            (map (fn [after-num]
                   {:num after-num :depth (inc (:depth node))})
                 (do-operations (:num node))))
          (next-level [nodes]
            (let [match (find-matching-node nodes)]
              (if match
                (:depth match)
                (recur (mapcat next-level-nodes nodes)))))]

    (next-level [{:num start :depth 1}])))


(is (= 1 (solve 1 1)))  ; 1
(is (= 3 (solve 3 12))) ; 3 6 12
(is (= 3 (solve 12 3))) ; 12 6 3
(is (= 3 (solve 5 9)))  ; 5 7 9
(is (= 9 (solve 9 2)))  ; 9 18 20 10 12 6 8 4 2
(is (= 5 (solve 9 12))) ; 9 11 22 24 12
