(use '[clojure.test :only (is)])

(println (str '((fn []
                  (let [q \"
                        l ["(fn []"
                           "(let [q \\\""]])))))

(is (= (str 'gus) (gus)))
