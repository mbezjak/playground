(ns stm.core)

(defn incr [r]
  (dosync
   (alter r + 1)))

(defn run []
  (let [r (ref 0)
        op #(incr r)]
    (println (pcalls op op op op))
    (op)
    (deref r)))
