(ns core)

(use 'debux.core)

(+ 1 (dbg (* 10 3)))

(defn my-fun
  [a {:keys [b c d] :or {d 10 b 20 c 30}} [e f g & h]]
  (dbg [a b c d e f g h]))

(my-fun (take 5 (range)) {:c 50 :d 100} ["a" "b" "c" "d" "e"])

(defn my-fun2
  [a {:keys [b c d] :or {d 10 b 20 c 30}} [e f g & h]]
  (dbgn [a b c d e f g h]))

(my-fun2 (take 5 (range)) {:c 50 :d 100} ["a" "b" "c" "d" "e"])

(def person
  {:name "Mark Volkmann"
   :address {:street "644 Glen Summit"
             :city "St. Charles"
             :state "Missouri"
             :zip 63304}
   :employer {:name "Object Computing, Inc."
              :address {:street "12140 Woodcrest Dr."
                        :city "Creve Coeur"
                        :state "Missouri"
                        :zip 63141}}})

(dbg (-> person :employer :address :city))

(def c 5)

(dbg (->> c (+ 3) (/ 2) (- 1)))

(->> [-1 0 1 2]
     (filter pos?)
     (map inc)
     dbg
     (map str))

(dbg (let [a (take 5 (range))
           {:keys [b c d] :or {d 10 b 20 c 30}} {:c 50 :d 100}
           [e f g & h] ["a" "b" "c" "d" "e"]]
        [a b c d e f g h]))

(def c (dbg (comp inc inc +)))

(c 10 20)
