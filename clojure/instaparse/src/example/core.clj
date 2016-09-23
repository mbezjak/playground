(ns example.core
  (:require [instaparse.core :as insta]
            [clojure.java.io])
  (:use [clojure.test :only (is)])
  (:gen-class))

(def as-and-bs
  (insta/parser
   "S = AB*
    AB = A B
    A = 'a'+
    B = 'b'+"))

(is (= (as-and-bs "aabbaaaabb")
       [:S
        [:AB [:A "a" "a"] [:B "b" "b"]]
        [:AB [:A "a" "a" "a" "a"] [:B "b" "b"]]]))

(def brackets
  (insta/parser
   "S = PS*
    PS = '(' (A | PS) ')'
    A = 'a'"))

(is (= (brackets "((a))")
       [:S
        [:PS "(" [:PS "(" [:A "a"] ")"] ")"]]))

(is (= (:reason (brackets "((a)"))
       [{:tag :string, :expecting ")"}]))

(def as
  (insta/parser (clojure.java.io/resource "as.bnf")))

(is (= (as "     a   ")
       [:S "     " "a" "   "]))

(def paren-ab
  (insta/parser
   "paren-wrapped = <'('> seq-of-A-or-B <')'>
    <seq-of-A-or-B> = ('a' | 'b')*"))

(is (= (paren-ab "(aab)")
       [:paren-wrapped "a" "a" "b"]))

(defn -main [& args])
