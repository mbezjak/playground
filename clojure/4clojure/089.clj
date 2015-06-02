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

(defn find-next-edges [graph node]
  (set (concat
        (filter #(= node (first %)) graph)
        (filter #(= node (first %)) (map reverse graph)))))

(is (= #{[:a :b]} (find-next-edges [[:a :b] [:b :c]] :a)))
(is (= #{[:a :b] [:a :c] (find-next-edges [[:a :b] [:b :c] [:a :c]] :a)}))
(is (= #{[:b :a] [:b :c] (find-next-edges [[:a :b] [:b :c] [:a :c]] :b)}))

(defn consume [[from to :as edge] remaining]
  (if (empty? remaining)
    true
    (let [next-edges (find-next-edges remaining to)]
      (if (empty? next-edges)
        false
        (not-every? false? (map (fn [next-edge]
                                  (consume next-edge (remove #{next-edge} remaining)))
                                next-edges))))))

(is (= true  (consume [:a :b] [])))
(is (= false (consume [:a :b] [[:c :d]])))
(is (= true  (consume [:a :b] [[:b :c]])))
(is (= false (consume [:a :b] [[:b :c] [:b :d]])))
(is (= true  (consume [:a :b] [[:b :c] [:c :d] [:b :d]])))

(defn tour? [graph]
  (if-not (apply distinct? graph)
    false
    (not-every? false? (for [starting-edge graph]
                         (consume starting-edge (remove #{starting-edge} graph))))))

(is (= true (tour? [[:a :b]])))

(is (= false (tour? [[:a :a] [:b :b]])))

(is (= false (tour? [[:a :b] [:a :b] [:a :c] [:c :a]
                     [:a :d] [:b :d] [:c :d]])))

(is (= true (tour? [[1 2] [2 3] [3 4] [4 1]])))

(is (= true (tour? [[:a :b] [:a :c] [:c :b] [:a :e]
                    [:b :e] [:a :d] [:b :d] [:c :e]
                    [:d :e] [:c :f] [:d :f]])))

(is (= false (tour? [[1 2] [2 3] [2 4] [2 5]])))
