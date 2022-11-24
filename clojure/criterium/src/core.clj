(ns core)

(use 'criterium.core)
(import 'java.util.UUID)

(quick-bench (Thread/sleep 1000))
(with-progress-reporting (quick-bench (+ 1 2) :verbose))
(with-progress-reporting (quick-bench (UUID/randomUUID) :verbose))
(quick-bench (UUID/randomUUID) :verbose)
(quick-bench (UUID/randomUUID))
(bench (UUID/randomUUID))

(quick-bench (into {} (filter (comp not nil? val) {:a 1 :b nil :c nil})))
(quick-bench (into {} (filter (fn [e] (not (nil? (val e)))) {:a 1 :b nil :c nil})))
(quick-bench (into {} (filter (fn [e] (not (nil? (second e)))) {:a 1 :b nil :c nil})))
(quick-bench (into {} (filter (fn [[_ v]] (not (nil? v))) {:a 1 :b nil :c nil})))
(quick-bench (into {} (remove (comp nil? val) {:a 1 :b nil :c nil})))
(quick-bench (into {} (remove (comp nil? val)) {:a 1 :b nil :c nil}))

(quick-bench {})
(quick-bench (into {} []))
(quick-bench (hash-map))
