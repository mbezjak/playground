(defn f [xs ys]
  (set
   (for [x xs
         y ys]
     [x y])))

(assert (= (f #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
           #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
             ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
             ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}))

(assert (= (f #{1 2 3} #{4 5})
           #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))

(assert (= 300 (count (f (into #{} (range 10))
                          (into #{} (range 30))))))
