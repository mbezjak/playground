(use '[clojure.test :only (is)])

(defn all-nodes-in [graph]
  (set (flatten graph)))

(is (= #{:a :b :c} (all-nodes-in [[:a :a] [:b :b] [:a :c]])))

(defn graph-to-map [graph]
  (apply merge-with concat (for [[key val] graph] {key [val]})))

(is (= (graph-to-map [[:a :a] [:a :b] [:b :c]])
       {:a [:a :b]
        :b [:c]}))

(defn remove-visited-node [node graph-hash]
  (->> (dissoc graph-hash node)
       (map (fn [[key links]] {key (remove #{node} links)}))
       (apply merge)
       (filter (fn [[key links]] (not (empty? links))))
       (into {})))

(is (= (remove-visited-node :a {:a [:a :b] :b [:a] :c [:b]})
       {:c [:b]}))

(defn consume [must-visit-nodes remaining-nodes graph-hash]
  (cond
    (and (empty? must-visit-nodes) (empty? remaining-nodes) (empty? graph-hash)) true
    (empty? must-visit-nodes) false
    :else
    (let [node (first must-visit-nodes)
          links (get graph-hash node)
          new-must-visit-nodes (set (concat (disj must-visit-nodes node) links))
          new-remaining-nodes (disj remaining-nodes node)
          new-graph-hash (remove-visited-node node graph-hash)]
      (consume new-must-visit-nodes new-remaining-nodes new-graph-hash))))

(is (= (consume #{:a} #{:a :b :c} {:a [:b], :b [:c]})
       true))

(is (= (consume #{:a} #{:a :b :c} {:a [:b], :b [:a]})
       false))

(is (= (consume #{:a} #{:a :b} {:a [:a], :b [:b]})
       false))

(is (= (consume #{1} #{1 2 3 4} {1 [2], 2 [3], 3 [4], 4 [1]})
       true))

(is (= (consume #{1} #{1 2 3 4 5} {1 [2], 2 [3 4 5]})
       false))

(defn tour? [graph]
  (let [all-nodes (all-nodes-in graph)
        graph-hash (graph-to-map graph)]
    (not-every? false? (for [starting-node all-nodes]
                         (consume #{starting-node} all-nodes graph-hash)))))

(is (= true (tour? [[:a :b]])))

(is (= false (tour? [[:a :a] [:b :b]])))

(is (= false (tour? [[:a :b] [:a :b] [:a :c] [:c :a]
                     [:a :d] [:b :d] [:c :d]])))

(is (= true (tour? [[1 2] [2 3] [3 4] [4 1]])))

(is (= true (tour? [[:a :b] [:a :c] [:c :b] [:a :e]
                    [:b :e] [:a :d] [:b :d] [:c :e]
                    [:d :e] [:c :f] [:d :f]])))

(is (= false (tour? [[1 2] [2 3] [2 4] [2 5]])))
