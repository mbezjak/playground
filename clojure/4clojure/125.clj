(use '[clojure.test :only (is)])
(require 'clojure.string)

(println ((fn []
            (let [q (char 34)
                  s (char 32)
                  l ["(fn []"
                     "(let [q (char 34)"
                     "s (char 32)"
                     "l ["
                     "]]"
                     "(str (clojure.string/join s (take 4 l))"
                     "(clojure.string/join s (map (fn [x] (str q x q)) l))"
                     "(clojure.string/join s (drop 4 l)))))"]]
              (str (clojure.string/join s (take 4 l))
                   (clojure.string/join s (map (fn [x] (str q x q)) l))
                   (clojure.string/join s (drop 4 l)))))))

(println)

(println (str '(fn []
                 (let [q (char 34)
                       s (char 32)
                       l ["(fn []"
                          "(let [q (char 34)"
                          "s (char 32)"
                          "l ["
                          "]]"
                          "(str (clojure.string/join s (take 4 l))"
                          "(clojure.string/join s (map (fn [x] (str q x q)) l))"
                          "(clojure.string/join s (drop 4 l)))))"]]
                   (str (clojure.string/join s (take 4 l))
                        (clojure.string/join s (map (fn [x] (str q x q)) l))
                        (clojure.string/join s (drop 4 l)))))))

(is (= (str '(fn []
               (let [q (char 34)
                     s (char 32)
                     l ["(fn []"
                        "(let [q (char 34)"
                        "s (char 32)"
                        "l ["
                        "]]"
                        "(str (clojure.string/join s (take 4 l))"
                        "(clojure.string/join s (map (fn [x] (str q x q)) l))"
                        "(clojure.string/join s (drop 4 l)))))"]]
                 (str (clojure.string/join s (take 4 l))
                      (clojure.string/join s (map (fn [x] (str q x q)) l))
                      (clojure.string/join s (drop 4 l))))))

       ((fn []
          (let [q (char 34)
                s (char 32)
                l ["(fn []"
                   "(let [q (char 34)"
                   "s (char 32)"
                   "l ["
                   "]]"
                   "(str (clojure.string/join s (take 4 l))"
                   "(clojure.string/join s (map (fn [x] (str q x q)) l))"
                   "(clojure.string/join s (drop 4 l)))))"]]
            (str (clojure.string/join s (take 4 l))
                 (clojure.string/join s (map (fn [x] (str q x q)) l))
                 (clojure.string/join s (drop 4 l))))))))
