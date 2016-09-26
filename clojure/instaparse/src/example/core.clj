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

(def words-and-numbers
  (insta/parser
   "sentence = token (<whitespace> token)*
    <token> = word | number
    whitespace = #'\\s+'
    word = #'[a-zA-Z]+'
    number = #'[0-9]+'"))

(is (= (words-and-numbers "abc 123 def")
       [:sentence [:word "abc"] [:number "123"] [:word "def"]]))

(def repeated-a
  (insta/parser "S = 'a'+"))

(is (= (set (insta/parses repeated-a "aaab" :partial true))
       (set [[:S "a"]
             [:S "a" "a"]
             [:S "a" "a" "a"]])))

(def lookahead-example
  (insta/parser
   "S = &'ab' ('a' | 'b')+"))

(is (= (lookahead-example "abaab")
       [:S "a" "b" "a" "a" "b"]))

(is (= (:reason (lookahead-example "bbaab"))
       [{:tag :string, :expecting "ab"}]))

(def same-length-abc
  (insta/parser
   "S = &(A 'c') 'a'+ B
    A = 'a' A? 'b'
    <B> = 'b' B? 'c'"))

(is (= (same-length-abc "aaabbbccc")
       [:S "a" "a" "a" "b" "b" "b" "c" "c" "c"]))

(is (= (:reason (same-length-abc "aabbbccc"))
       [{:tag :string, :expecting "c"}]))

(defn -main [& args])
