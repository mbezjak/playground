(require '[clojure.set])

(def x #{1 2})

(assert (clojure.set/superset? x #{2}))
(assert (clojure.set/subset? #{1} x))
(assert (clojure.set/superset? x #{1 2}))
(assert (clojure.set/subset? #{1 2} x))
