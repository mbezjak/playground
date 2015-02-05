(defn f [human-representation]
  (let [[s r] human-representation
        map-suite {\S :spades, \H :heart, \D :diamond, \C :club}
        map-rank {\2 0, \3 1, \4 2, \5 3, \6 4, \7 5, \8 6, \9 7, \T 8, \J 9, \Q 10, \K 11, \A 12}]

    {:suit (map-suite s), :rank (map-rank r)}))

(assert (= {:suit :diamond :rank 10} (f "DQ")))
(assert (= {:suit :heart :rank 3} (f "H5")))
(assert (= {:suit :club :rank 12} (f "CA")))
(assert (= (range 13) (map (comp :rank f str)
                           '[S2 S3 S4 S5 S6 S7
                             S8 S9 ST SJ SQ SK SA])))
