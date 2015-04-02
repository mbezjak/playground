(use '[clojure.test :only (is)])

(defn game [trump]
  (fn [cards]
    (letfn [(similar [suit]
              (filter #(= suit (get % :suit)) cards))
            (leader []
              (get (first cards) :suit))
            (top-rank [suit]
              (last (sort-by :rank (similar suit))))]

      (if trump
        ((game nil) (similar trump))
        (top-rank (leader))))))

(is (let [notrump (game nil)]
      (and (= {:suit :club :rank 9}  (notrump [{:suit :club :rank 4}
                                               {:suit :club :rank 9}]))
           (= {:suit :spade :rank 2} (notrump [{:suit :spade :rank 2}
                                               {:suit :club :rank 10}])))))

(is (= {:suit :club :rank 10} ((game :club) [{:suit :spade :rank 2}
                                             {:suit :club :rank 10}])))

(is (= {:suit :heart :rank 8}
       ((game :heart) [{:suit :heart :rank 6} {:suit :heart :rank 8}
                       {:suit :diamond :rank 10} {:suit :heart :rank 4}])))
