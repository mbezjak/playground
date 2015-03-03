(use '[clojure.test :only (is)])

(defn anagrams [words]
  (->> (group-by frequencies words)
       (vals)
       (filter #(< 1 (count %)))
       (map set)
       (set)))

(is (= (anagrams ["meat" "mat" "team" "mate" "eat"])
       #{#{"meat" "team" "mate"}}))

(is (= (anagrams ["veer" "lake" "item" "kale" "mite" "ever"])
       #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}}))
