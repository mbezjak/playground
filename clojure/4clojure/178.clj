(use '[clojure.test :only (is)])
(require 'clojure.set)

(defn describe [human-representation]
  (let [[s r] human-representation
        map-suite {\S :spades, \H :heart, \D :diamond, \C :club}
        map-rank {\2 0, \3 1, \4 2, \5 3, \6 4, \7 5, \8 6, \9 7, \T 8, \J 9, \Q 10, \K 11, \A 12}]

    {:suit (map-suite s), :rank (map-rank r)}))

(defn same-suit? [cards]
  (= 1 (count (distinct (map :suit cards)))))

(is (= true  (same-suit? [{:suit :heart, :rank 8} {:suit :heart, :rank 9}, {:suit :heart, :rank 10}])))
(is (= false (same-suit? [{:suit :club,  :rank 8} {:suit :heart, :rank 9}, {:suit :heart, :rank 10}])))

(defn in-sequence? [cards]
  (let [ranks (sort (map :rank cards))
        start (first ranks)]
    (or
     (= ranks (range start (+ start (count cards))))
     (if (.contains ranks 12)
       (in-sequence? (map (fn [card] (if (= 12 (:rank card))
                                      {:suit (:suit card), :rank -1}
                                      card)) cards))
       false))))

(is (= true  (in-sequence? [{:suit :club, :rank 8} {:suit :heart, :rank 9}, {:suit :heart, :rank 10}])))
(is (= true  (in-sequence? [{:suit :club, :rank 12} {:suit :heart, :rank 0}, {:suit :heart, :rank 1}])))
(is (= false (in-sequence? [{:suit :club, :rank 7} {:suit :heart, :rank 9}, {:suit :heart, :rank 10}])))

(defn highest-rank [cards]
  (last (sort (map :rank cards))))

(is (= 10 (highest-rank [{:suit :club, :rank 8} {:suit :heart, :rank 9}, {:suit :heart, :rank 10}])))

(defn has-rank-count? [count cards]
  (contains? (clojure.set/map-invert (frequencies (map :rank cards))) count))

(is (= true  (has-rank-count? 2 [{:suit :club, :rank 8} {:suit :heart, :rank 8}, {:suit :heart, :rank 10}])))
(is (= true  (has-rank-count? 1 [{:suit :club, :rank 8} {:suit :heart, :rank 8}, {:suit :heart, :rank 10}])))
(is (= false (has-rank-count? 3 [{:suit :club, :rank 8} {:suit :heart, :rank 8}, {:suit :heart, :rank 10}])))

(defn two-pairs? [cards]
  (= 2 (get (frequencies (vals (frequencies (map :rank cards)))) 2)))

(defn recognize [cards-human]
  (let [cards (mapv describe cards-human)]
    (cond
      (and (same-suit? cards) (in-sequence? cards) (= 12 (highest-rank cards))) :straight-flush
      (has-rank-count? 4 cards) :four-of-a-kind
      (and (has-rank-count? 3 cards) (has-rank-count? 2 cards)) :full-house
      (same-suit? cards) :flush
      (in-sequence? cards) :straight
      (has-rank-count? 3 cards) :three-of-a-kind
      (two-pairs? cards) :two-pair
      (has-rank-count? 2 cards) :pair
      :else :high-card)))

(defn recognize-4clojure [cards-human]
  (letfn [(describe [human-representation]
            (let [[s r] human-representation
                  map-suite {\S :spades, \H :heart, \D :diamond, \C :club}
                  map-rank {\2 0, \3 1, \4 2, \5 3, \6 4, \7 5, \8 6, \9 7, \T 8, \J 9, \Q 10, \K 11, \A 12}]

              {:suit (map-suite s), :rank (map-rank r)}))
          (same-suit? [cards]
            (= 1 (count (distinct (map :suit cards)))))
          (in-sequence? [cards]
            (let [ranks (sort (map :rank cards))
                  start (first ranks)]
              (or
               (= ranks (range start (+ start (count cards))))
               (if (.contains ranks 12)
                 (in-sequence? (map (fn [card] (if (= 12 (:rank card))
                                                {:suit (:suit card), :rank -1}
                                                card)) cards))
                 false))))
          (highest-rank [cards]
            (last (sort (map :rank cards))))
          (has-rank-count? [count cards]
            (contains? (clojure.set/map-invert (frequencies (map :rank cards))) count))
          (two-pairs? [cards]
            (= 2 (get (frequencies (vals (frequencies (map :rank cards)))) 2)))]
    (let [cards (mapv describe cards-human)]
      (cond
        (and (same-suit? cards) (in-sequence? cards) (= 12 (highest-rank cards))) :straight-flush
        (has-rank-count? 4 cards) :four-of-a-kind
        (and (has-rank-count? 3 cards) (has-rank-count? 2 cards)) :full-house
        (same-suit? cards) :flush
        (in-sequence? cards) :straight
        (has-rank-count? 3 cards) :three-of-a-kind
        (two-pairs? cards) :two-pair
        (has-rank-count? 2 cards) :pair
        :else :high-card))))


(is (= :high-card (recognize ["HA" "D2" "H3" "C9" "DJ"])))
(is (= :pair (recognize ["HA" "HQ" "SJ" "DA" "HT"])))
(is (= :two-pair (recognize ["HA" "DA" "HQ" "SQ" "HT"])))
(is (= :three-of-a-kind (recognize ["HA" "DA" "CA" "HJ" "HT"])))
(is (= :straight (recognize ["HA" "DK" "HQ" "HJ" "HT"])))
(is (= :straight (recognize ["HA" "H2" "S3" "D4" "C5"])))
(is (= :flush (recognize ["HA" "HK" "H2" "H4" "HT"])))
(is (= :full-house (recognize ["HA" "DA" "CA" "HJ" "DJ"])))
(is (= :four-of-a-kind (recognize ["HA" "DA" "CA" "SA" "DJ"])))
(is (= :straight-flush (recognize ["HA" "HK" "HQ" "HJ" "HT"])))
