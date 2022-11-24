(ns core)

;;  https://functional.works-hub.com/learn/a-guide-to-the-clojure-spec-library-606d6
(require '[clojure.string :as string])
(require '[clojure.spec.alpha :as s])
(require '[clojure.spec.test.alpha :as stest])
(require '[clojure.spec.gen.alpha :as gen])

(s/valid? zero? 0)
(s/valid? zero? 1)
(s/valid? #{:a :b :c} :c)
(s/valid? #{:a :b :c} :d)

(defn punctuated? [s]
  "Check if a string ends with a full-stop, question or exclamation mark."
  (let [punctuation #{"." "?" "!"}]
    (some (partial string/ends-with? s) punctuation)))

(s/valid? punctuated? "Hello world!")
(s/valid? punctuated? "Hello world")
(s/explain-data punctuated? "Hello world")

(s/def ::punctuated? punctuated?)
(doc ::punctuated?)
(s/explain-data ::punctuated? "Hello world")

(defn starts-with-capital? [s]
  "Check if given word, or sentence starts with a capital letter."
  (Character/isUpperCase (first s)))
(s/def ::starts-with-capital? starts-with-capital?)

(s/def ::proper-sentence? (s/and ::punctuated? ::starts-with-capital?))

(s/valid? ::proper-sentence? "Hello there")
(s/valid? ::proper-sentence? "hello there!")
(s/valid? ::proper-sentence? "Hello there!")
(s/explain-data ::proper-sentence? "hello there!")

(s/def ::noun? ::starts-with-capital?)

(defn good-addition?
  [sentence]
  (some (partial string/includes? sentence) #{"Furthermore" "Therefore" "In addition" "Moreover"}))

(defn bad-addition?
  [sentence]
  (some (partial string/includes? sentence) #{"And" "The" "Because"}))

(s/def ::good-addition? good-addition?)
(s/def ::bad-addition? bad-addition?)
(s/def ::check-addition (s/or :good ::good-addition? :bad ::bad-addition?))

(s/valid? ::check-addition "Furthermore")
(s/conform ::check-addition "Furthermore")
(s/conform ::starts-with-capital? "Hello")
(s/conform ::starts-with-capital? "ello")
(s/conform (s/or :pass ::check-addition :fail identity) "Furthermo")
