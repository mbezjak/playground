(defn f [& args]
  (and (boolean (some true? args))
       (not (every? true? args))))

(assert (= false (f false false)))
(assert (= true  (f true false)))
(assert (= false (f true)))
(assert (= true  (f false true false)))
(assert (= false (f true true true)))
(assert (= true  (f true true true false)))
