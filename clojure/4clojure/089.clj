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

(defn remove-visited-edge [[from to] graph-hash]
  (let [rm-link (fn [key val map]
                  (let [links (get map key)]
                    (if links
                      (assoc map key (remove #{val} links))
                      map)))]
    (->> graph-hash
         (rm-link from to)
         (rm-link to from)
         (filter (fn [[key links]] (not (empty? links))))
         (into {}))))

(is (= (remove-visited-edge [:a :a] {:a [:a :b] :b [:a] :c [:b]})
       {:a [:b] :b [:a] :c [:b]}))

(is (= (remove-visited-edge [:a :b] {:a [:a :b] :b [:a] :c [:b]})
       {:a [:a] :c [:b]}))

(defn consume [node remaining-nodes graph-hash]
  (if (empty? remaining-nodes)
    true
    (let [links (get graph-hash node)
          new-graph-hash (remove-visited-node node graph-hash)]
      (if (and (nil? links) (not (empty? remaining-nodes)))
        false
        (not-every? false? (map (fn [link]
                                  (consume link (disj remaining-nodes link) new-graph-hash))
                                links))))))

(is (= true  (consume :a #{:b :c}  {:a [:b], :b [:c]})))
(is (= false (consume :a #{:b :c}  {:a [:b], :b [:a]})))
(is (= false (consume :a #{:b}     {:a [:a], :b [:b]})))
(is (= true  (consume 1  #{2 3 4}   {1 [2], 2 [3], 3 [4], 4 [1]})))
(is (= false (consume 1  #{2 3 4 5} {1 [2], 2 [3 4 5]})))
(is (= false (consume :a #{:b :c :d} {:a [:b :b :c :d], :b [:d], :c [:a :d]})))
(is (= false (consume :b #{:a :c :d} {:a [:b :b :c :d], :b [:d], :c [:a :d]})))
(is (= false (consume :c #{:a :b :d} {:a [:b :b :c :d], :b [:d], :c [:a :d]})))

(defn tour? [graph]
  (let [all-nodes (all-nodes-in graph)
        graph-hash (graph-to-map graph)]
    (not-every? false? (for [starting-node all-nodes]
                         (consume starting-node (disj all-nodes starting-node) graph-hash)))))

(is (= true (tour? [[:a :b]])))

(is (= false (tour? [[:a :a] [:b :b]])))

(is (= false (tour? [[:a :b] [:a :b] [:a :c] [:c :a]
                     [:a :d] [:b :d] [:c :d]])))

(is (= true (tour? [[1 2] [2 3] [3 4] [4 1]])))

(is (= true (tour? [[:a :b] [:a :c] [:c :b] [:a :e]
                    [:b :e] [:a :d] [:b :d] [:c :e]
                    [:d :e] [:c :f] [:d :f]])))

(is (= false (tour? [[1 2] [2 3] [2 4] [2 5]])))
