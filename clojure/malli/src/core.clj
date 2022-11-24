(ns core)

(require '[malli.core :as m])
(require '[malli.provider :as mp])

(def schema
  [:map
   [:a {:optional true} int?]])

(def schema
  [:map {:closed true}
   [:a {:optional true} int?]])

(m/explain schema {:a 1})
(m/explain schema {:b 2})
(m/explain schema {:a :1})
(m/validate schema {:a :1})
(m/validate [:and int? [:> 5]] "a")
(m/explain [:and int? [:> 5]] 1)
(m/validate [:and int? [:> 5]] 6)


(mp/provide [[1]])
(mp/provide [[1 2 3 :a]])
(mp/provide [[{:a 1} {:a 2}]])
(mp/provide [[{:a 1} {:b 2}]])
