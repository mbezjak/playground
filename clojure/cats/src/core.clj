(ns core)

;; https://github.com/funcool/cats
(require '[cats.core :as m])
(require '[cats.builtin])
(require '[cats.context :as ctx])
(require '[cats.monad.maybe :as maybe])
(require '[cats.monad.either :as either])
(require '[cats.monad.exception :as exc])

(m/mappend [1 2 3] [4 5 6])
(type (m/mappend [1 2 3] [4 5 6]))
(m/mappend [] [])

(m/mappend (maybe/just [1 2 3]) (maybe/just [4 5 6]))
(type (m/mappend (maybe/just [1 2 3]) (maybe/just [4 5 6])))
(supers (type (m/mappend (maybe/just [1 2 3]) (maybe/just [4 5 6]))))

(m/mappend (maybe/just [1 2 3])
           (maybe/nothing)
           (maybe/just [4 5 6])
           (maybe/nothing))

(m/mempty)
(m/mempty cats.builtin/set-context)
(m/mempty cats.builtin/map-context)
(m/mempty cats.builtin/function-context)
(m/mempty cats.builtin/sum-monoid)
(m/mempty cats.builtin/all-monoid)

(m/fmap inc [1 2 3])
(m/fmap inc (maybe/just 3))
(m/fmap inc (maybe/nothing))

(m/pure maybe/context 5)
(m/pure 5)

(m/fapply (maybe/just inc) (maybe/just 5))
(m/fapply (maybe/just inc) (maybe/nothing))
(m/fapply (maybe/nothing) (maybe/just 5))
(m/fapply (maybe/nothing) (either/right 5))
(m/fapply (maybe/just inc) (either/right 5))
(m/fapply (maybe/just inc) (either/left 5))
(m/fapply (either/left inc) (either/left 3))
(m/fapply (either/right inc) (either/left 3))
(m/fapply (either/right inc) (either/right 3))

(m/foldl + 0 [1 2 3])
(m/foldl + 1 (maybe/just 3))
(m/foldl + 1 (either/right 3))
(m/foldl + 1 (either/left 3))
(m/foldl #(m/return (+ %1 %2)) 1 (maybe/just 1))
(m/foldl #(m/return (+ %1 %2)) 1 (maybe/nothing))

(defn m-div [x y]
  (if (zero? y)
    (maybe/nothing)
    (maybe/just (/ x y))))
(m/foldm m-div 1 [1 2 3])
(m/foldm m-div 1 [1 0 3])

(defn just-if-even [n]
  (if (even? n)
    (maybe/just n)
    (maybe/nothing)))
(ctx/with-context maybe/context (m/traverse just-if-even []))
(ctx/with-context maybe/context (m/traverse just-if-even [2 4]))
(ctx/with-context maybe/context (m/traverse just-if-even [2 3 4]))

(m/bind (maybe/just 1) #(m/return (inc %)))
(m/bind (maybe/just 1) just-if-even)
(m/bind (maybe/just 2) just-if-even)
(m/bind (maybe/just 1)
        (fn [a]
          (m/bind (maybe/just (inc a))
                  (fn [b]
                    (m/return (* b 2))))))
(m/mlet [a (maybe/just 1)
         b (maybe/just (inc a))]
        (m/return (* b 2)))

(m/mzero maybe/context)
(m/mzero either/context)
(m/mzero cats.builtin/set-context)
(m/mzero cats.builtin/vector-context)

(m/mlet [a (maybe/just 2)
         :when (= a 2)]
        (m/return (* a 2)))

(m/mplus (maybe/nothing))
(m/mplus (maybe/just 1))
(m/mplus (maybe/just 1) (maybe/just 2))
(m/mplus (maybe/just 1) (maybe/nothing))
(m/mplus (maybe/nothing) (maybe/just 1))

(maybe/from-maybe (maybe/nothing))
(maybe/from-maybe (maybe/just 1))
@(maybe/just 1)
@(maybe/nothing)

@(either/left 1)
@(either/right 2)

(exc/success 1)
(exc/failure (Exception. "blah"))
(exc/try-on 1)
(exc/try-on (/ 1 0))
(exc/try-or-else (+ 1 nil) 2)
(exc/try-or-recover (+ 1 nil)
                    (fn [e]
                      (cond
                        (instance? NullPointerException e) 0
                        :else 100)))
@(exc/try-on (+ 1 nil))
(class nil)
(type nil)

(m/return maybe/context (maybe/just 1))
(m/pure maybe/context 1)
(m/pure maybe/context (maybe/just 1))
(m/pure (maybe/just 1))
(m/bind (maybe/just 1) identity)
(m/join (maybe/just 1))
(m/join (maybe/nothing))
(ctx/with-context maybe/context (m/join 1))
(maybe/maybe 0 (maybe/just 1) inc)
(maybe/maybe 0 (maybe/nothing) inc)
(maybe/maybe 0 1 inc)

(m/mlet [x (maybe/just 42)
         y nil]
        (m/return (+ x y)))
(m/mlet [x (either/left 1)]
        (inc x))
(m/mlet [x (either/right 1)]
        (inc x))
(m/alet [x (either/left 1)]
        (inc x))
(m/alet [x (either/right 1)]
        (inc x))
(m/mappend {:a 1} {:b 2})
(m/mappend {:a 1} {:a 2})
(m/mappend {:a 1} {:a nil})
(m/mempty cats.builtin/map-context)

(m/mapseq maybe/just [2 3])
(maybe/just [2 3])
(m/sequence [(either/right 1) (either/right 2)])
(m/sequence [(either/left 1) (either/right 2)])
(m/sequence [(either/right 1) (either/left 2)])
