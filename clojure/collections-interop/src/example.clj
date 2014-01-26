(def j (java.util.ArrayList.))
(dotimes [i 5] (.add j i))

(def c [0 1 2 3 4])

(defn to-clojure [x]
  (into [] x))

(defn to-java [x]
  (java.util.ArrayList. x))


(assert (= java.util.ArrayList (type j)))
(assert (= clojure.lang.PersistentVector (type c)))

(assert (= c j))
(assert (= c (to-clojure j)))
(assert (= (to-java c) j))
