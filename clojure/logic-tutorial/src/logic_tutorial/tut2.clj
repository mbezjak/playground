(ns logic-tutorial.tut2
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic :exclude [appendo membero]]))

(defn appendo [l1 l2 o]
  (conde
    ((== l1 ()) (== l2 o))
    ((fresh [a d r]
       (conso a d l1)
       (conso a r o)
       (appendo d l2 r)))))

(defn membero [x l]
  (conde
   [(firsto l x)]
   [(fresh [a r]
      (conso a r l)
      (membero x r))]))

(defne membero2
  [x l]
  ([_ [x . _]])
  ([_ [_ . r]] (membero2 x r)))
