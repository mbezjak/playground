(ns core)

(require '[clojure.datafy :refer [datafy]])
(require '[clojure.inspector :as inspector])
(datafy 1)
(datafy {:a 1})
(datafy java.lang.String)
(datafy clojure.lang.IFn)
(datafy clojure.core.protocols.Datafiable)
(datafy (the-ns 'clojure.core))
(datafy (the-ns 'clojure.inspector))
(type *ns*)
(inspector/inspect (datafy clojure.core.protocols.Datafiable))
(inspector/inspect-tree (datafy clojure.core.protocols.Datafiable))
(datafy *ns*)
(java.util.ArrayList. 16)
(type (datafy (java.util.ArrayList. 16)))


(def tpl "%s is %s")
(format tpl)
(format tpl "a" "b")
(apply format tpl ["a" "b"])
(apply format tpl ["a"])
(apply format tpl ["a" nil])
(apply format tpl ["a" "b" "c"])
(apply format "a is b" nil)
(apply format "a is b" [])
(format "Hello %s")
(apply format "Hello %s" [])
(format "Hello %s" "world" "galaxy" "universe")
(apply format "Hello %s" ["world" "galaxy" "universe"])


(def tf (.toFormatter
         (doto
             (java.time.format.DateTimeFormatterBuilder.)
             (.appendInstant))))
(.format tf (java.time.Instant/now))
(.format tf (java.time.ZonedDateTime/now))
(.lengthOfYear (java.time.LocalDate/now))
(.lengthOfMonth (java.time.LocalDate/now))
(java.time.Month/from (java.time.LocalDate/now))

(= (apply + (repeat 8 0.1)) (* 0.1 8))

(defrecord Boundary [low-le low-lt high-ge high-gt])
(Boundary. 0 nil 100 nil)
(->Boundary 0 nil 100 nil)
(map->Boundary {:low-le 0 :high-ge 100})

(java.util.TimeZone/getDefault)

(def types [{:code "AAA" :name "A"}
            {:code "BBB" :name "B"}])
(mapv (juxt :name :code) types)
(mapv #(vector (:name %) (:code %)) types)


(take 20 (map inc (range)))
(take 20 (drop 1 (range)))
(/ 1 2)


(do
  (def m (Object.))
  (future (locking m
            (println "Before first")
            (Thread/sleep 5000)
            (println "End first")))
  (future
    (Thread/sleep 1000)
    (locking m
      (println "End second")))
  (locking (Object.)
    (println "Completely different monitor")))

(java.util.Optional/of nil)
(java.util.Optional/of 1)
(def o (java.util.Optional/of 1))
(System/identityHashCode o)
(Integer/toHexString (System/identityHashCode o))
(java.util.Optional/ofNullable "abc")
(java.util.Objects/hashCode (java.util.Optional/ofNullable nil))
(.hashCode (java.util.Optional/ofNullable nil))

(def j (new java.util.StringJoiner "," "{" "}"))
(doto j
  (.add "foo")
  (.add "bar")
  (.add "qux"))
(str j)

(def d (java.util.Base64/getEncoder))
(def d (java.util.Base64/getUrlEncoder))
(.encodeToString d (.getBytes "$abc#" "UTF-8"))
(.encodeToString (.withoutPadding d) (.getBytes "$abc#" "UTF-8"))

(def c (java.util.Currency/getInstance "HRK"))
(.getCurrencyCode c)
(.getDefaultFractionDigits c)
(.getDisplayName c)
(.getNumericCode c)
(.getSymbol c)

(def q (new java.util.concurrent.ArrayBlockingQueue 5))
(.offer q :a)
(.offer q :b 5 java.util.concurrent.TimeUnit/SECONDS)
(vec (.toArray q))
(.size q)
(.remove q)
(.poll q)
(.poll q 5 java.util.concurrent.TimeUnit/SECONDS)
(.clear q)

(def q (new java.util.PriorityQueue 5))
(.offer q :a)
(vec (.toArray q))
(.size q)
(.remove q)
(.poll q)
(.clear q)

(def t (new java.util.TreeSet [5 1 2 4 3]))
(.ceiling t 7)
(.lower t 7)
(.pollFirst t)
(.tailSet t 3)

(java.util.Collections/disjoint [1 2] [3])
(java.util.Collections/disjoint [1 2] [1 3])
(doto (new java.util.ArrayList ["a" "b" "c"])
  (java.util.Collections/fill "a"))
(java.util.Collections/indexOfSubList [1 2 3 4 5] [3 4 5])
(java.util.Collections/indexOfSubList [1 2 3 4 5] [4 4 5])
(java.util.Collections/nCopies 5 "a")
(java.util.Collections/newSetFromMap {:a true :b false :c true})
(doto (new java.util.ArrayList ["a" "b" "c"])
  (java.util.Collections/rotate 1))

(def a (doto (new java.util.BitSet)
         (.set 0 true)
         (.set 1 false)
         (.set 2 true)))
(def b (doto (new java.util.BitSet)
         (.set 0 true)
         (.set 1 true)
         (.set 2 true)
         (.set 3 false)))
(.length a)
(.size a)
(.size b)
(vec (.toByteArray a))
(.toString b)
(.xor a b)
(.cardinality a)
(.cardinality b)
(.intersects a b)
(.intersects a (doto b (.clone) (.clear)))


(java.util.Objects/hashCode (new java.util.ArrayList))
(java.util.Objects/hashCode (new String))
(java.util.Objects/hashCode (.toString (range 1 200)))
(java.util.Objects/hashCode "abc")
(java.util.Objects/hashCode [1 2])
(java.util.Objects/hashCode (reify Object))
(java.util.Objects/toString (reify Object))
(java.util.Objects/toString nil)

(. java.lang.System getProperties)
(def m "getProperties")
(def c "java.lang.System")
(def f (eval `(fn [] (. ~(symbol c) ~(symbol m)))))
(f)
(eval `(. ~(symbol c) ~(symbol m)))

[1 2]
(.iterator [1 2])
(.next (.iterator [1 2]))
(.remove (.iterator [1 2]))


(def m {:a 1 :b 2})
(into {} (map reverse m))
(into {} (map (comp vec reverse) m))
(conj {} [:a 1])
(conj {} '(:a 1))
(conj {} '(1 :a))
(instance? clojure.lang.IEditableCollection {})
(seq '(1 :a))
(map type m)


(defmulti foo :type)
(defmethod foo :a [m] "a")
(foo {:type :a})
(foo {:type :b})

(def xf (map str))
(transduce xf str #{:a :b :c})
(eduction xf #{:a :b :c})

(def l [:z :e :a :c :e :z])
(set l)
(distinct l)

(def c
  (reify CharSequence
    (charAt [this index] \a)))
(nth c 3)
(= 1 1.0)
(== 0.3 0.3)
(= (int 1) (long 1))
(= (int 1) (double 1))

(require '[clojure.set :as set])
(set/rename-keys {:a 1 :b 2} {:a :z})
(set/rename-keys {:a 1 :b 2} {:a2 :z})

(require '[clojure.string :as str])
(def currency-atom (atom nil))
(defn as-currency
  "Money amounts are transmitted as \"$2.44\".
 Parse this and return a numeric type."
  [currency-amount]
  (reset! currency-atom currency-amount)
  (let [negative? (re-find #"^[^\d]*-" currency-amount)
        cleaned-amount (str/replace currency-amount #"^[^\d\.]*" "")]
    (bigdec
     (str (when negative? "-") cleaned-amount))))

(as-currency "$2.44")
(as-currency "-$2.44")
@currency-atom
(def currency-amount "-$2.44")
(def negative? false)
(def cleaned-amount "2.44")

(-> 1
    (inc)
    ((fn [x] (println x) x)))
((fn [x] (println x) x))

(subs "$2.44" 1)

(read-string "1")
(read-string "+")
(type (read-string "+"))
(read-string "(+ 1 1)")
(eval (read-string "(+ 1 1)"))
(require '[clojure.edn :as edn])
(def a (edn/read-string "(+ 1 1)"))
`~a
`+
(defn ret-a []
  `(fn [] ~a))
(ret-a)
((ret-a))
(eval (ret-a))
((eval (ret-a)))
(edn/read-string {:readers {'foo/inc (fn [x] (inc x))}}
                 "{:a #foo/inc 1}")
*data-readers*
(edn/read-string {:readers {'foo/fn (fn [form]
                                      (let [pred (eval `(fn [~'v] ~form))]
                                        (pred 5)))}}
                 "{:pred #foo/fn (> v 6)}")

String


(defn divisible? [n d]
  (zero? (mod n d)))
(defn fizzbuzz [x]
  (cond
    (and (divisible? x 3)
         (divisible? x 5)) "FizzBuzz"
    (divisible? x 3) "Fizz"
    (divisible? x 5) "Buzz"
    :else x))
(for [x (range 1 100)]
  (fizzbuzz x))


(require '[clojure.test.check.generators :as gen])
(gen/sample (gen/choose 5 9))
(gen/sample (gen/elements ["clojure" "haskell" "erlang" "scala" "python"]))
(gen/sample (gen/one-of [gen/int (gen/return nil)]))
(gen/sample (gen/frequency [[9 gen/int] [1 (gen/return nil)]]))
(gen/sample (gen/fmap #(* 2 %) gen/pos-int))
(gen/sample gen/int)
(gen/sample (gen/fmap #(int (Math/pow 2 %)) gen/s-pos-int))
(gen/sample (gen/fmap (comp vec sort) (gen/vector gen/int)))
(gen/sample (gen/tuple gen/int gen/boolean))
(gen/sample (gen/such-that #(not= % 5) gen/int))
(gen/sample (gen/bind (gen/not-empty (gen/vector gen/int))
                      #(gen/tuple (gen/return %) (gen/elements %))))
(gen/sample gen/any)
(gen/sample gen/ratio)
(gen/sample (gen/set gen/ratio))
(gen/sample gen/double)
(gen/sample gen/any-printable)
(gen/sample gen/char-ascii)
(gen/sample (gen/frequency [[9 gen/char-alpha] [1 (gen/elements [\č \Č \ć \Ć \š \Š \đ \Đ \ž \Ž])]]))
(gen/sample gen/string)
(gen/sample gen/any)
(gen/sample gen/simple-type)
(gen/sample gen/any-printable)
(gen/sample gen/boolean)
(gen/sample gen/byte)
(gen/sample gen/bytes)
(gen/sample gen/char)
(gen/sample gen/char-alpha)
(gen/sample gen/char-alphanumeric)
(gen/sample gen/char-ascii)
(gen/sample (gen/choose 2 3))
(gen/sample (gen/choose 0.1 3))
(gen/sample (gen/container-type gen/int))
(gen/sample gen/double)
(gen/sample (gen/double* {:min 1}))
(gen/sample (gen/elements [\a \b \c]))
(gen/sample (gen/frequency [[9 gen/int] [1 (gen/return nil)]]))
(gen/generate gen/int)
(gen/generator? gen/int)
(gen/generator? gen/choose)
(gen/sample (gen/hash-map :name gen/string :value gen/any))
(gen/sample gen/int)
(gen/sample gen/keyword)
(gen/sample gen/keyword-ns)
(gen/sample gen/large-integer 100)
(gen/sample (gen/large-integer* {:min 100}))
(gen/sample gen/int 100)
(gen/sample (gen/let [strings (gen/not-empty (gen/vector gen/string))
                      s (gen/elements strings)]
              {:strings strings
               :one s}))
(gen/sample (gen/list-distinct gen/int))
(gen/sample (gen/list-distinct (gen/large-integer* {:min 1 :max 100})))
(gen/sample (gen/list-distinct (gen/large-integer* {:min 1 :max 2})))
(gen/sample (gen/list-distinct-by :name (gen/hash-map :name (gen/elements [:foo :bar :baz]) :age gen/nat)))
(gen/sample (gen/map gen/keyword gen/int {:min-elements 4}))
(gen/sample gen/nat 100)
(gen/sample gen/neg-int)
(gen/sample (gen/one-of [gen/boolean gen/int]))
(gen/sample gen/pos-int 100)
(gen/sample gen/ratio)
(gen/sample (gen/return 1))
(gen/sample gen/s-neg-int)
(gen/sample gen/s-pos-int)
(take 10 (gen/sample-seq gen/nat))
(gen/sample gen/simple-type)
(gen/sample (gen/set gen/int))
(gen/sample (gen/sorted-set gen/int))
(gen/sample gen/symbol)
(gen/sample gen/symbol-ns)
(gen/sample (gen/tuple gen/int gen/boolean))
(gen/sample gen/uuid)

(require '[clojure.test.check :as tc])
(require '[clojure.test.check.properties :as prop])
(defn ascending? [coll]
  (every? (fn [[a b]] (<= a b))
          (partition 2 1 coll)))
(ascending? [1 2 3])
(ascending? [1 3 2])
(def property
  (prop/for-all [v (gen/vector gen/int)]
                (let [s (sort v)]
                  (and (= (count v) (count s))
                       (ascending? s)))))
(tc/quick-check 100 property)
(tc/quick-check 100 property :seed 1507197297186)
(tc/quick-check 100 property :max-size 2000)
(def bad-property
  (prop/for-all [v (gen/vector gen/int)]
                (ascending? v)))
(tc/quick-check 100 bad-property)
(take 300 (gen/sample-seq gen/int))
(take 300 (gen/sample-seq gen/large-integer))
(gen/sample (gen/map gen/keyword gen/int))
(gen/sample (gen/fmap set (gen/vector gen/int)))
(gen/sample (gen/set gen/symbol))
(gen/sample (gen/fmap symbol gen/string-alphanumeric))
(gen/sample (gen/such-that not-empty (gen/list gen/int)))
(gen/sample (gen/not-empty (gen/vector gen/int)))
(gen/sample (gen/bind (gen/not-empty (gen/vector gen/keyword))
                      #(gen/tuple (gen/return %) (gen/elements %))))
(defrecord User [user-name user-id email active?])
(->User "reiddraper" 15 "reid@example.com" true)
(map->User {:user-name "abc"})
(map->User [[:user-name "abc"]])
(dissoc (map->User [[:user-name "abc"]]) :user-name)
(type (dissoc (map->User [[:user-name "abc"]]) :user-name))
(keys (map->User [[:user-name "abc"]]))
(map vec {:a 1 :b 2})
(->User "reiddraper" 15 "reid@example.com" true)
(def domain (gen/elements ["gmail.com" "hotmail.com" "computer.org"]))
(def email-gen
  (gen/fmap (fn [[name domain-name]]
              (str name "@" domain-name))
            (gen/tuple (gen/not-empty gen/string-alphanumeric) domain)))
(gen/sample email-gen)
(def user-gen
  (gen/fmap (partial apply ->User)
            (gen/tuple (gen/not-empty gen/string-alphanumeric)
                       gen/nat
                       email-gen
                       gen/boolean)))
(gen/sample user-gen)
(gen/sample (gen/recursive-gen gen/vector gen/boolean))
(def compound (fn [inner-gen]
                (gen/one-of [(gen/list inner-gen)
                             (gen/map inner-gen inner-gen)])))
(def scalars (gen/one-of [gen/int gen/boolean]))
(gen/sample scalars)
(gen/sample (gen/recursive-gen compound scalars))

(def dsl ['a
          'b :nullable
          'c
          'd :nullable :hidden
          'e
          'f
          'g
          'h {:width 20}
          'i])
(partition 2 1 dsl)
(partition-by #(if (symbol? %) % :attr) dsl)
(partition-by (fn [[n maybe-attr]] (if (symbol? n) n maybe-attr))
  (partition 2 1
    (partition-by #(if (symbol? %) % :attr) dsl)))

(in-ns 'user)
(defmacro infix [infixed]
  (list (second infixed) (first infixed) (last infixed)))
(infix [1 + 2])
(infix (1 + 2))
(1 + 2)
(defmacro infix-2 [[op1 op op2]]
  (list op op1 op2))
(infix-2 (1 + 2))
(defmacro my-and
  ([] true)
  ([x] x)
  ([x & more] `(let [and# ~x] (if and# (my-and ~@more) and#))))
(my-and)
(my-and 1)
(my-and 1 2)
(my-and false (repeat 1))
(my-and 1 2 3)
(defmacro my-print-whoopsie [expression]
  `(let [result# ~expression]
     (println `(= ~result# ~'~expression))
     result#))
(my-print-whoopsie (+ 1 2))
(+ 1 2)
(quote (+ 1 2))
'(+ 1 2)
+
(quote +)
(defmacro unless [test body]
  (list 'if (list 'not test)
        (list 'do body)))
(unless (contains? {} :a)
        {:a 1})
`(+ 1 2)
`(+ 1 (inc 1))
`(+ 1 ~(inc 1))
(list '+ 1 (inc 1))
(defmacro code-critic [bad good]
  `(do (println "Bad:" '~bad)
       (println "Good:" '~good)))
(code-critic (1 + 1) (+ 1 1))
(defn criticize-code [desc code]
  `(println ~desc '~code))
(defmacro code-critic [bad good]
  `(do ~(criticize-code "Bad:" bad)
       ~(criticize-code "Good:" good)))
(defmacro code-critic [bad good]
  `(do ~@(map #(apply criticize-code %)
              [["Bad:" bad]
               ["Good:" good]])))
(code-critic (1 + 1) (+ 1 1))
(def message "Good job!")
(defmacro with-mischief [& stuff-to-do]
  (concat (list 'let ['message "Oh big deal!"])
          stuff-to-do))
(with-mischief
  (println "Message: " message))
(defmacro with-mischief [& stuff-to-do]
  `(let [message "Oh big deal!"]
     ~@stuff-to-do))
(defmacro with-mischief [& stuff-to-do]
  (let [macro-message (gensym 'message)]
    `(let [~macro-message "Oh big deal!"]
       ~@stuff-to-do)))
(defmacro with-mischief [& stuff-to-do]
  `(let [message# "Oh big deal!"]
     ~@stuff-to-do))
(gensym)
(defmacro report [to-try]
  `(if ~to-try
     (println (quote ~to-try) "was successful:" ~to-try)
     (println (quote ~to-try) "was not successful:" ~to-try)))
(time (report (do (Thread/sleep 1000) (+ 1 1))))
(defmacro report [to-try]
  `(let [result# ~to-try]
     (if result#
       (println (quote ~to-try) "was successful:" result#)
       (println (quote ~to-try) "was not successful:" result#))))
(def order-details
  {:name "John Doe"
   :email "john.doegmail.com"})
(def order-details-validations
  {:name
   ["Please enter a name" not-empty]

   :email
   ["Please enter an email address" not-empty
    "Your email address doesn't look like an email address"
    #(or (empty? %) (re-seq #"@" %))]})
(defn error-messages-for [to-validate message-validator-pairs]
  (map first (filter #(not ((second %) to-validate))
                     (partition 2 message-validator-pairs))))
(error-messages-for "" ["Please enter a name" not-empty])
(error-messages-for "John" ["Please enter a name" not-empty])
(defn validate
  "Returns a map with a vector of errors for each key"
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[fieldname validation-check-groups] validation
                  value (get to-validate fieldname)
                  error-messages (error-messages-for value validation-check-groups)]
              (if (empty? error-messages)
                errors
                (assoc errors fieldname error-messages))))
          {}
          validations))
(validate order-details order-details-validations)
(let [errors (validate order-details order-details-validations)]
  (if (empty? errors)
    (println :success)
    (println :failure errors)))
(defn if-valid
  [record validations success-code failure-code]
  (let [errors (validate record validations)]
    (if (empty? errors)
      success-code
      failure-code)))
(defmacro if-valid
  "Handle validation more concisely"
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
       ~@then-else)))
(if-valid order-details order-details-validations errors
 (println :success)
 (println :failure errors))

'+
'foo
`foo
(namespace 'bar)
(namespace `bar)
(namespace 'foo/bar)
(name 'bar)
(name `bar)
(name "ewq")
(def x 42)
(var-get #'x)
(defn increment [x] (inc x))
increment
(var-get #'increment)
(increment 0)
(alter-var-root #'increment (partial comp increment))
(increment 0)
(def ^:dynamic y 0)
y
(binding [y 42]
  y)
y
(import '[clojure.lang Var])
(Var/create 42)
(var-get (Var/create 42))
(let [x (.setDynamic (Var/create 0))]
  (println (var-get x))
  (with-bindings {x 42}
    (println (var-get x)))
  (println (var-get x)))
(var 'foo)
(var `+)
(var +)
(class '+)
(def a '+)
(var a)
(find-var a)
(find-var `+)
(find-var '+)
(find-var +)
*ns*
(in-ns 'clojure.test)
(println *ns*)
(in-ns 'user)
*ns*
(ns example.foo)
(ns example.bar)
(in-ns 'user)
(find-ns 'example.foo)
(find-ns 'example.bar)
(find-ns 'example.baz)
(find-ns (find-ns 'example.foo))
(the-ns 'example.foo)
(the-ns 'example.baz)
(the-ns (the-ns 'example.foo))
(ns user)
@@#'clojure.core/*loaded-libs*
(ns example.foo)
(ns example.bar
  (:require [example.foo :as ef]))
(ns-aliases 'example.foo)
(ns-aliases 'example.bar)
(defn ^:private multiply [x y]
  (* x y))
(defn add [x y]
  (+ x y))
(ns-map 'example.bar)
(ns-publics 'example.bar)
(defn my-apply [fs x y]
  (let [f (var-get (get (ns-publics *ns*) fs))]
    (f x y)))
(my-apply 'add 1 2)
(defn my-apply [fs x y]
  (let [f (var-get (find-var fs))]
    (f x y)))
(my-apply `add 1 2)
(ns example.core)
(defn increment [x] (+ 1 x))
(increment 1)
(ns example.repl
  (:require [example.core :as core :refer [increment]]))
(increment 1)
(defn resolve-symbol [s]
  (if-let [namespace-name (namespace s)]
    (when-let [referenced-namespace (or (get (ns-aliases *ns*) (symbol namespace-name))
                                        (find-ns (symbol namespace-name)))]
      (get (ns-publics referenced-namespace) (symbol (name s))))
    (get (ns-map *ns*) s)))
(resolve-symbol 'increment)
(resolve-symbol 'core/increment)
(resolve-symbol 'example.core/increment)
(defn symbol-apply [s args]
  (when-let [referenced-var (resolve-symbol s)]
    (apply (var-get referenced-var) args)))
(symbol-apply 'increment [1])
(symbol-apply 'core/increment [1])
(symbol-apply 'example.core/increment [1])


(in-ns 'user)
(require '[clojure.test :refer :all])
(is (= 4 (+ 2 2)))
(is (= 4 (+ 2 3)))
(is 5)
(is (thrown? ArithmeticException (/ 1 0)))
(testing "Arithmetic"
  (is (= 4 (+ 1 2))))
(with-test
  (defn my-function [x y]
    (+ x y))
  (is (= 4 (my-function 2 2))))
(deftest addition
  (is (= 4 (+ 2 2)))
  (is (= 5 (+ 1 4))))
(deftest subtraction
  (is (= 1 (- 3 2)))
  (is (= 0 (- 1 1))))
(deftest arithmetic
  (addition)
  (subtraction))
(arithmetic)
(run-tests)
(use-fixtures :once
  (fn [f] (println "Before 1") (f) (println "After 1"))
  (fn [f] (println "Before 2") (f) (println "After 2")))
(are [x y] (= x y)
  2 (+ 1 1)
  4 (* 2 2))
(ns-interns *ns*)

(defn foo [x] (inc x))
(foo 2)
(alter-var-root #'foo (constantly (fn [x] (dec x))))
(foo 3)

(in-ns 'user)
#{(inc 1)}
(def m (sorted-map :hx-operater 1 :nalog 2 :banka 3))
(type m)
(assoc m :a 1)
(type (assoc m :a 1))
(type (set [1 2]))
(type (hash-set 1 2))
(type (sorted-set 1 2))


(def form '#(* 2 4))
(form)
(.invoke (eval form))
(def missing '#(this-does-not-exist))
(missing)
(eval missing)


(defmacro quote-name [name]
  `(quote ~name))
(quote-name foo)
(defmacro quote-name [name]
  `'~name)
(quote-name foo)

(defmacro quote-name [name]
  (println (type name))
  `(quote ~name))
(quote-name foo)

(defmacro quote-name [name]
  `(quote ~(type name)))
(quote-name foo)

(update {} :a inc)

(java.io.File. "/tmp")
(java.util.concurrent.ArrayBlockingQueue. 5)
(def f (java.io.File. "/tmp"))
(iterator-seq f)
(enumeration-seq f)
(def files (file-seq f))
(count (filter (memfn isDirectory) files))


(def m {:name :foo
        :doc "A foo"
        :version "1.0"})
((every-pred :name :doc) m)
((every-pred :name :doc) (dissoc m :doc))
((some-fn :name :doc) m)
((comp boolean (some-fn :name :doc)) m)
((some-fn :doc :name) (dissoc m :doc))

(fn? :a)
(ifn? :a)
(fn? #(1))

(supers clojure.lang.Keyword)
(supers clojure.lang.Symbol)
(supers java.lang.String)

(with-meta :a {:foo 1})
(with-meta 'a {:foo 1})


(defmacro qux [form] form)
(qux abc)
(defmacro bar [form] (type form))
(bar abc)
(defmacro foo [form] `(quote ~form))
(foo a.b.c)
(type (foo a.b.c))
(macroexpand '(foo a.b.c))


(set/union nil nil #{})
(not-empty #{})
(not-empty #{:a})
(not-empty [])

{:a 1 :a 2}

(type (/ 1.0 3.0))
(type 1.0)
(type 1M)
(type 1.0M)

((comp str inc) 1)
(apply (comp str inc first) [1 :a])
((comp str inc (apply first)) [1 :a])

((comp (partial apply (fn [a b] (+ a b))) (juxt :a :b)) {:a 1 :b 2})

(update {} :a conj 1)
(update {} :a (fnil conj []) 1)

(defn rename-key [map from-key to-key]
  (if (contains? map from-key)
    (-> map
        (assoc to-key (get map from-key))
        (dissoc from-key))
    map))
(contains? {:a 1} :a)
(contains? {:a 1} :b)
(assoc {:a 1} :b (get {:a 1} :a))
(rename-key {} :a :b)
(rename-key {:a 1} :a :b)

(supers (class (get {:pred true?} :pred)))
(str (get {:pred true?} :pred))
(resolve (symbol (get {:pred true?} :pred)))
(:name (meta (resolve (symbol "true?"))))
(str (get {:pred "true?"} :pred))
(name (get {:pred true?} :pred))
(:name (meta #'true?))
(def a {:pred true?})
(def b (get a :pred))
(var b)
(var (get a :pred))
(let [pred (get a :pred)]
  (str pred))
(with-local-vars [pred (get a :pred)]
  (meta @pred))
(require '[clojure.main :as m])
(require '[clojure.string :as s])
(s/replace (m/demunge (str (get a :pred))) #"@[0-9a-f]+" "")
true?
(var (get {:pred true?} :pred))
(str true?)
(str (var true?))
(meta #'true?)
(:name (meta #'true?))
(:name (meta (var true?)))
(:ns (meta #'true?))
(ns-name (:ns (meta #'true?)))
(= 'clojure.core (ns-name (:ns (meta #'true?))))
(meta (:ns (meta #'true?)))

((resolve '+) 1 2 3)
(@(resolve '+) 1 2 3)

(var +)
+
@(var +)
(= + @(var +))
