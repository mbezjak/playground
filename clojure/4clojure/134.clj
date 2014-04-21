(defn f [key map]
  (and (.containsKey map key) (nil? (map key))))

(assert (true? (f :a {:a nil :b 2})))
(assert (false? (f :b {:a nil :b 2})))
(assert (false? (f :c {:a nil :b 2})))
