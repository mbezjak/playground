(defn f [s]
  (clojure.string/join "" (re-seq #"[A-Z]" s)))

(assert (= (f "HeLlO, WoRlD!") "HLOWRD"))
(assert (empty? (f "nothing")))
(assert (= (f "$#A(*&987Zf") "AZ"))
