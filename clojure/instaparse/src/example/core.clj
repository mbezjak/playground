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

(is (= (paren-ab "(aab)" :unhide :content)
       [:paren-wrapped "(" "a" "a" "b" ")"]))

(def right-recursion
  (insta/parser "S = 'a' S | Epsilon"))

(is (= (right-recursion "aaaa")
       [:S "a" [:S "a" [:S "a" [:S "a" [:S]]]]]))

(def left-recursion
  (insta/parser "S = 'a'*"))

(is (= (left-recursion "aaaa")
       [:S "a" "a" "a" "a"]))

(def ambiguous
  (insta/parser
   "S = A A
    A = 'a'*"))

(is (= (ambiguous "aaaa")
       [:S
        [:A "a"]
        [:A "a" "a" "a"]]))

(is (= (set (insta/parses ambiguous "aaaa"))
       (set [[:S [:A] [:A "a" "a" "a" "a"]]
             [:S [:A "a"] [:A "a" "a" "a"]]
             [:S [:A "a" "a"] [:A "a" "a"]]
             [:S [:A "a" "a" "a"] [:A "a"]]
             [:S [:A "a" "a" "a" "a"] [:A]]])))

(defn -main [& args])
