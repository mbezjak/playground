(use '[clojure.test :only (is)])

(defn filter-final-states [frontier accepted-states]
  (into {} (filter #(contains? accepted-states (key %)) frontier)))

(is (= '{a "1", c "3"} (filter-final-states '{a "1", b "2", c "3"} (set '[a c]))))

(defn transition [frontier transitions]
  (for [[state sentences] frontier
        :when (contains? transitions state)
        :let [alphabet->state (get transitions state)]]
    [state (mapv (fn [sentence] #()) sentences)]))

(is (= (transition '{a "1", b "2"} '{a {1 a}
                                     b {2 b, 1 a}})
       '{a ["11"]
         b ["22" "21"]}))

(is (= (transition '{a "1", b "2"} '{a {1 a}})
       '{a ["11"]}))

ee

(defn next-step [{:keys [accepts]} frontier]
  (let [final (filter-final-states frontier accepts)]
    (lazy-cat (map val final)
              )))

(defn recognized [dfa]
  (let [[final continue] (partition-by #(contains? % accepted-states) frontier)]
    (lazy-cat (map #(get-string-for-final) final)
              (map #(recognize dfa frontier) continue)
              (map #(recognize dfa frontier) final))))

(is (= #{"a" "ab" "abc"}
       (set (recognized '{:states #{q0 q1 q2 q3}
                          :alphabet #{a b c}
                          :start q0
                          :accepts #{q1 q2 q3}
                          :transitions {q0 {a q1}
                                        q1 {b q2}
                                        q2 {c q3}}}))))


(is (= #{"hi" "hey" "hello"}
       (set (recognized '{:states #{q0 q1 q2 q3 q4 q5 q6 q7}
                          :alphabet #{e h i l o y}
                          :start q0
                          :accepts #{q2 q4 q7}
                          :transitions {q0 {h q1}
                                        q1 {i q2, e q3}
                                        q3 {l q5, y q4}
                                        q5 {l q6}
                                        q6 {o q7}}}))))

(is (= (set (let [ss "vwxyz"] (for [i ss, j ss, k ss, l ss] (str i j k l))))
       (set (recognized '{:states #{q0 q1 q2 q3 q4}
                          :alphabet #{v w x y z}
                          :start q0
                          :accepts #{q4}
                          :transitions {q0 {v q1, w q1, x q1, y q1, z q1}
                                        q1 {v q2, w q2, x q2, y q2, z q2}
                                        q2 {v q3, w q3, x q3, y q3, z q3}
                                        q3 {v q4, w q4, x q4, y q4, z q4}}}))))

(let [res (take 2000 (recognized '{:states #{q0 q1}
                                   :alphabet #{0 1}
                                   :start q0
                                   :accepts #{q0}
                                   :transitions {q0 {0 q0, 1 q1}
                                                 q1 {0 q1, 1 q0}}}))]
  (is (and (every? (partial re-matches #"0*(?:10*10*)*") res)
           (= res (distinct res)))))

(let [res (take 2000 (recognized '{:states #{q0 q1}
                                   :alphabet #{n m}
                                   :start q0
                                   :accepts #{q1}
                                   :transitions {q0 {n q0, m q1}}}))]
  (is (and (every? (partial re-matches #"n*m") res)
           (= res (distinct res)))))

(let [res (take 2000 (recognized '{:states #{q0 q1 q2 q3 q4 q5 q6 q7 q8 q9}
                                   :alphabet #{i l o m p t}
                                   :start q0
                                   :accepts #{q5 q8}
                                   :transitions {q0 {l q1}
                                                 q1 {i q2, o q6}
                                                 q2 {m q3}
                                                 q3 {i q4}
                                                 q4 {t q5}
                                                 q6 {o q7}
                                                 q7 {p q8}
                                                 q8 {l q9}
                                                 q9 {o q6}}}))]
  (is (and (every? (partial re-matches #"limit|(?:loop)+") res)
           (= res (distinct res)))))
