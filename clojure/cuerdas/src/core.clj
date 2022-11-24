(ns core)

;; https://github.com/funcool/cuerdas
(require '[cuerdas.core :as str])

(str/strip-tags "<p>just <b>some</b> text</p>")
(str/strip-tags "<p>just <b>some</b> text<p>")
(str/strip-tags "<p>just <script>alert('ewq');</script> text<p>")

(str/<<- "a
    b
  c")

(def value 30)
(def map {:a 1})
(str/istr "value = ~{value}")
(str/istr "value = ~(inc (:a map)")
(str/istr "value = ~(inc (:a map))")

(str/capital "some random phrase")
(str/title "some random phrase")
(str/human :some-keyword)
(str/camel :some-keyword)
(str/camel :some-a-keyword)
(str/keyword "someKeyword")
(str/keyword "asomeKeyword")
(str/keyword "aSomeKeyword")
(str/lines "a\nb")
(str/lines "a\r\nb")
(str/numeric? "1.1")
(str/numeric? "-1.1")
(str/numeric? "+1.1")
(str/numeric? "+1.1a")
(str/parse-double "a")
(class (str/parse-double "a"))
(str/slug "%a")
(str/to-bool "false")
(str/to-bool "a")
(str/to-bool "on")
