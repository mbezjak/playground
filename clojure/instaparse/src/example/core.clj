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

(def negative-lookahead-example
  (insta/parser
   "S = !'ab' ('a' | 'b')+"))

(is (= (:reason (negative-lookahead-example "abaaab"))
       [{:tag :negative-lookahead, :expecting {:NOT "\"ab\""}}]))

(def ambiguous-tokenizer
  (insta/parser
   "sentence = token (<whitespace> token)*
    <token> = keyword | identifier
    whitespace = #'\\s+'
    identifier = #'[a-zA-Z]+'
    keyword = 'cond' | 'defn'"))

(is (= (set (insta/parses ambiguous-tokenizer "defn my cond"))
       (set [[:sentence [:identifier "defn"] [:identifier "my"] [:identifier "cond"]]
             [:sentence [:keyword "defn"] [:identifier "my"] [:identifier "cond"]]
             [:sentence [:identifier "defn"] [:identifier "my"] [:keyword "cond"]]
             [:sentence [:keyword "defn"] [:identifier "my"] [:keyword "cond"]]])))

(def unambiguous-tokenizer
  (insta/parser
   "sentence = token (<whitespace> token)*
    <token> = keyword | !keyword identifier
    whitespace = #'\\s+'
    identifier = #'[a-zA-Z]+'
    keyword = 'cond' | 'defn'"))

(is (= (insta/parses unambiguous-tokenizer "defn my cond")
       [[:sentence [:keyword "defn"] [:identifier "my"] [:keyword "cond"]]]))

(def preferential-tokenizer
  (insta/parser
   "sentence = token (<whitespace> token)*
    <token> = keyword / identifier
    whitespace = #'\\s+'
    identifier = #'[a-zA-Z]+'
    keyword = 'cond' | 'defn'"))

(is (= (first (insta/parses preferential-tokenizer "defn my cond"))
       [:sentence [:keyword "defn"] [:identifier "my"] [:keyword "cond"]]))

;; using get-failure
(is (not (nil? (insta/get-failure (insta/parses preferential-tokenizer "1 does not parse")))))

;; total parse mode
(is (= (repeated-a "aabaaa" :total true)
       [:S "a" "a" [:instaparse/failure "baaa"]]))

(is (insta/failure? (repeated-a "aabaa" :total true)))

(is (= (:reason (insta/get-failure (repeated-a "aabaa" :total true)))
       [{:tag :string, :expecting "a"}]))

;; another start rule
(is (= (as-and-bs "aaa" :start :A)
       [:A "a" "a" "a"]))

(def words-and-numbers-one-character-at-a-time
  (insta/parser
   "sentence = token(<whitespace> token)*
    <token> = word | number
    whitespace = #'\\s+'
    word = letter+
    number = digit+
    <letter> = #'[a-zA-Z]'
    <digit> = #'[0-9]'"))

(is (= (words-and-numbers-one-character-at-a-time "abc 123 def")
       [:sentence [:word "a" "b" "c"] [:number "1" "2" "3"] [:word "d" "e" "f"]]))

(is (= (insta/transform
        {:word str
         :number (comp clojure.edn/read-string str)}
        (words-and-numbers-one-character-at-a-time "abc 123 def"))
       [:sentence "abc" 123 "def"]))

(def arithmetic
  (insta/parser
   "expr = add-sub
    <add-sub> = mul-div | add | sub
    add = add-sub <'+'> mul-div
    sub = add-sub <'-'> mul-div
    <mul-div> = term | mul | div
    mul = mul-div <'*'> term
    div = mul-div <'/'> term
    <term> = number | <'('> add-sub <')'>
    number = #'[0-9]+'"))

(is (= (arithmetic "1-2/(3-4)+5*6")
       [:expr
        [:add
         [:sub
          [:number "1"]
          [:div [:number "2"] [:sub [:number "3"] [:number "4"]]]]
         [:mul [:number "5"] [:number "6"]]]]))

(is (= (->> (arithmetic "1-2/(3-4)+5*6")
            (insta/transform
             {:add +, :sub -, :mul *, :div /
              :number clojure.edn/read-string
              :expr identity}))
       33))

;; transform of parses or failure
(is (= (->> (insta/parses ambiguous "aaaa")
            (insta/transform {:A str}))
       [[:S "a" "aaa"]
        [:S "aaaa" ""]
        [:S "aa" "aa"]
        [:S "aaa" "a"]
        [:S "" "aaaa"]]))

(is (= (->> (insta/parses ambiguous "aabaa")
            (insta/get-failure)
            (insta/transform {:A str})
            (:reason))
       [{:tag :string, :expecting "a"}]))

(is (= (insta/span (as-and-bs "aabbbaaaaabb"))
       [0 12]))

(def multiline-text "This is line 1\nThis is line 2")

(is (= (words-and-numbers multiline-text)
       [:sentence
        [:word "This"] [:word "is"] [:word "line"] [:number "1"]
        [:word "This"] [:word "is"] [:word "line"] [:number "2"]]))

(def parsed-multiline-text-with-line-and-column-metadata
  (insta/add-line-and-column-info-to-metadata
   multiline-text
   (words-and-numbers multiline-text)))

(is (= parsed-multiline-text-with-line-and-column-metadata
       [:sentence
        [:word "This"] [:word "is"] [:word "line"] [:number "1"]
        [:word "This"] [:word "is"] [:word "line"] [:number "2"]]))

(is (= (meta parsed-multiline-text-with-line-and-column-metadata)
       {:instaparse.gll/start-column 1
        :instaparse.gll/start-line 1
        :instaparse.gll/start-index 0
        :instaparse.gll/end-column 15
        :instaparse.gll/end-line 2
        :instaparse.gll/end-index 29}))

(is (= ((insta/parser "S = 'a'+" :string-ci true) "AaaAaa")
       [:S "a" "a" "a" "a" "a" "a"]))

(is (= ((insta/parser "S = #'(?i)a'+") "AaaAaa")
       [:S "A" "a" "a" "A" "a" "a"]))

(defn -main [& args]
  (if (and (not (empty? args))
           (= (first args) "visualize"))
    (insta/visualize (as-and-bs "aaabbab"))))
