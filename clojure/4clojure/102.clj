(defn capitalize [[c & cs]]
  (str (Character/toUpperCase c) (apply str cs)))

(defn intoCamelCase [name]
  (let [[first & rest] (clojure.string/split name #"-")]
    (str first (apply str (map capitalize rest)))))

(assert (= (intoCamelCase "something") "something"))
(assert (= (intoCamelCase "multi-word-key") "multiWordKey"))
(assert (= (intoCamelCase "leaveMeAlone") "leaveMeAlone"))
