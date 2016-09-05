(ns example.core
  (:require [instaparse.core :as insta]
            [clojure.java.io])
  (:gen-class))

(def as-and-bs
  (insta/parser
   "S = AB*
    AB = A B
    A = 'a'+
    B = 'b'+"))

(def brackets
  (insta/parser
   "S = PS*
    PS = '(' (A | PS) ')'
    A = 'a'"))

(def as
  (insta/parser (clojure.java.io/resource "as.bnf")))

(defn -main [& args]
  (println (as-and-bs "aaaaabbbbaaaaaabb"))
  (println (brackets "((a)"))
  (println (brackets "((a))"))
  (println (as "   a  ")))
