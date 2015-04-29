(use '[clojure.test :only (is)])

(defn transitive [relations]
  (letfn [(to-map [rels]
            (apply merge-with concat (map (fn [[l r]] {l [r]}) rels)))
          (new-rels-from [map]
            (for [[src dsts] map
                  dst dsts
                  :when (contains? map dst)
                  :let [new-dsts (get map dst)]
                  new-dst new-dsts]
              [src new-dst]))
          (produce-from [rels]
            (let [new-rels (new-rels-from (to-map rels))
                  all-rels (set (concat rels new-rels))]
              (if (= rels all-rels)
                all-rels
                (produce-from all-rels))))]
    (produce-from relations)))

(is (let [divides #{[8 4] [9 3] [4 2] [27 9]}]
      (= (transitive divides) #{[4 2] [8 4] [8 2] [9 3] [27 9] [27 3]})))

(is (let [more-legs
          #{["cat" "man"] ["man" "snake"] ["spider" "cat"]}]
      (= (transitive more-legs)
         #{["cat" "man"] ["cat" "snake"] ["man" "snake"]
           ["spider" "cat"] ["spider" "man"] ["spider" "snake"]})))

(is (let [progeny
          #{["father" "son"] ["uncle" "cousin"] ["son" "grandson"]}]
      (= (transitive progeny)
         #{["father" "son"] ["father" "grandson"]
           ["uncle" "cousin"] ["son" "grandson"]})))
