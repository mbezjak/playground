(ns core)

;; https://weavejester.github.io/medley/medley.core.html
(require '[medley.core :as m])

(m/abs -1)
(abs -1)
(m/assoc-some {} :a nil)
(m/assoc-some {} :a 1)
(m/dedupe-by :id [{:id 1} {:id 1} {:id 2}])
(m/distinct-by :id [{:id 1} {:id 2} {:id 1}])
(m/deep-merge {:a 1 :n {:b 2 :e []}}
              {:c 3 :n {:b 3 :d 4 :e [1]}})
