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

(def paren-ab
  (insta/parser
   "paren-wrapped = <'('> seq-of-A-or-B <')'>
    <seq-of-A-or-B> = ('a' | 'b')*"))

(defn -main [& args]
  (println (as-and-bs "aaaaabbbbaaaaaabb"))
  (println (brackets "((a)"))
  (println (brackets "((a))"))
  (println (as "   a  "))
  (println (paren-ab "(aab)")))
