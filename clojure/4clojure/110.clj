(use '[clojure.test :only (is)])

(defn pronunciations [coll]
  (let [pronunce (->> (partition-by identity coll)
                       (mapcat (fn [els] [(count els) (first els)])))]
    (lazy-seq (cons pronunce (pronunciations pronunce)))))

(= [[1 1] [2 1] [1 2 1 1]] (take 3 (pronunciations [1])))
(= [3 1 2 4] (first (pronunciations [1 1 1 4 4])))
(= [1 1 1 3 2 1 3 2 1 1] (nth (pronunciations [1]) 6))
(= 338 (count (nth (pronunciations [3 2]) 15)))
