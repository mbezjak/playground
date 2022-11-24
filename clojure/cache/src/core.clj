(ns core)

;; https://github.com/clojure/core.cache
(require '[clojure.core.cache :as cache])
(def C1 (cache/fifo-cache-factory {:a 1 :b 2}))
(def C1' (if (cache/has? C1 :c)
           (cache/hit C1 :c)
           (cache/miss C1 :c 42)))
(cache/lookup C1' :c)

(def C1' (cache/through-cache C1 :c (constantly 42)))
(cache/evict C1 :b)
