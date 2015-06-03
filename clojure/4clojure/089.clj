(use '[clojure.test :only (is)])

(defn find-next-edges [graph node]
  (set (concat
        (filter #(= node (first %)) graph)
        (filter #(= node (first %)) (map reverse graph)))))

(is (= #{[:a :b]} (find-next-edges [[:a :b] [:b :c]] :a)))
(is (= #{[:a :b] [:a :c] (find-next-edges [[:a :b] [:b :c] [:a :c]] :a)}))
(is (= #{[:b :a] [:b :c] (find-next-edges [[:a :b] [:b :c] [:a :c]] :b)}))

(defn remove-from-graph [[from to] graph]
  (remove #{[from to] [to from]} graph))

(is (= [[:a :c]] (remove-from-graph [:a :b] [[:a :b] [:b :a] [:a :c]])))

(defn consume [[from to] remaining]
  (if (empty? remaining)
    true
    (let [next-edges (find-next-edges remaining to)]
      (if (empty? next-edges)
        false
        (not (empty? (drop-while false? (map (fn [next-edge]
                                               (consume next-edge (remove-from-graph next-edge remaining)))
                                             next-edges))))))))

(is (= true  (consume [:a :b] [])))
(is (= false (consume [:a :b] [[:c :d]])))
(is (= true  (consume [:a :b] [[:b :c]])))
(is (= false (consume [:a :b] [[:b :c] [:b :d]])))
(is (= true  (consume [:a :b] [[:b :c] [:c :d] [:b :d]])))
(is (= true  (consume [:d :f] [[:a :b] [:a :c] [:c :b] [:a :e]
                               [:b :e] [:a :d] [:b :d] [:c :e]
                               [:d :e] [:c :f]])))

(defn tour? [graph]
  (if-not (apply distinct? graph)
    false
    (not (empty? (drop-while false? (for [starting-edge graph]
                                      (consume starting-edge (remove #{starting-edge} graph))))))))

(defn tour-for-4clojure? [orig-graph]
  (letfn [(find-next-edges [graph node]
            (set (concat
                  (filter #(= node (first %)) graph)
                  (filter #(= node (first %)) (map reverse graph)))))
          (remove-from-graph [[from to] graph]
            (remove #{[from to] [to from]} graph))
          (consume [[from to] remaining]
            (if (empty? remaining)
              true
              (let [next-edges (find-next-edges remaining to)]
                (if (empty? next-edges)
                  false
                  (not (empty? (drop-while false? (map (fn [next-edge]
                                                         (consume next-edge (remove-from-graph next-edge remaining)))
                                                       next-edges))))))))]
    (if-not (apply distinct? orig-graph)
      false
      (not (empty? (drop-while false? (for [starting-edge orig-graph]
                                        (consume starting-edge (remove #{starting-edge} orig-graph)))))))))

(is (= true (tour? [[:a :b]])))

(is (= false (tour? [[:a :a] [:b :b]])))

(is (= false (tour? [[:a :b] [:a :b] [:a :c] [:c :a]
                     [:a :d] [:b :d] [:c :d]])))

(is (= true (tour? [[1 2] [2 3] [3 4] [4 1]])))

(is (= true (tour? [[:a :b] [:a :c] [:c :b] [:a :e]
                    [:b :e] [:a :d] [:b :d] [:c :e]
                    [:d :e] [:c :f] [:d :f]])))

(is (= false (tour? [[1 2] [2 3] [2 4] [2 5]])))
