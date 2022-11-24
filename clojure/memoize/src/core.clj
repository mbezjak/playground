(ns core)

;; https://github.com/clojure/core.memoize
(require '[clojure.core.memoize :as memoize])
(def id (memoize/lu #(do (Thread/sleep 5000) (identity %))
                    :lu/threshold 3))

(id 41)
(id 42)
(id 43)
(id 44)

(memoize/memoized? id)
(memoize/memo-clear! id)

(def id' (memoize/memo-unwrap id))
(id' 41)
