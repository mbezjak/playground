(use '[clojure.test :only (is)])

(defn victor [board]
  (let [lines (concat board
                      (for [i (range 3)] (map #(nth % i) board))
                      (vector (for [i (range 3)] (get-in board [i i])))
                      (vector (for [i (range 3)] (get-in board [i (- 3 1 i)]))))
        is-winner (fn [player] (some #(every? #{player} %) lines))]
    (cond
      (is-winner :x) :x
      (is-winner :o) :o
      :else nil)))


(is (= nil (victor [[:e :e :e]
                    [:e :e :e]
                    [:e :e :e]])))

(is (= :x (victor [[:x :e :o]
                   [:x :e :e]
                   [:x :e :o]])))

(is (= :o (victor [[:e :x :e]
                   [:o :o :o]
                   [:x :e :x]])))

(is (= nil (victor [[:x :e :o]
                    [:x :x :e]
                    [:o :x :o]])))

(is (= :x (victor [[:x :e :e]
                   [:o :x :e]
                   [:o :e :x]])))

(is (= :o (victor [[:x :e :o]
                   [:x :o :e]
                   [:o :e :x]])))

(is (= nil (victor [[:x :o :x]
                    [:x :o :x]
                    [:o :x :o]])))
