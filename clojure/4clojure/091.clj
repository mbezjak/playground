(use '[clojure.test :only (is)])
(require '[clojure.set])

(defn connected? [graph]
  (let [all-nodes (set (flatten (vec graph)))
        nodes-map (reduce #(merge-with concat %1 %2) (for [[x y] graph] {x [y]}))
        connected (fn connected [remaining node]
                    (if (empty? remaining)
                      []
                      (let [links (get remaining node)
                            new-remaining (dissoc remaining node)
                            links-to-node (for [[up-key up-links] remaining
                                                :when (some #{node} up-links)]
                                            up-key)]
                        (set (concat [node]
                                     links
                                     (mapcat #(connected new-remaining %) links)
                                     (mapcat #(connected new-remaining %) links-to-node))))))]

    (= all-nodes (connected nodes-map (first all-nodes)))))

(is (= true (connected? #{[:a :a]})))

(is (= true (connected? #{[:a :b]})))

(is (= false (connected? #{[1 2] [2 3] [3 1]
                           [4 5] [5 6] [6 4]})))

(is (= true (connected? #{[1 2] [2 3] [3 1]
                          [4 5] [5 6] [6 4] [3 4]})))

(is (= false (connected? #{[:a :b] [:b :c] [:c :d]
                           [:x :y] [:d :a] [:b :e]})))

(is (= true (connected? #{[:a :b] [:b :c] [:c :d]
                          [:x :y] [:d :a] [:b :e] [:x :a]})))
