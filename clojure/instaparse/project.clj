(defproject instaparse "0.1.0-SNAPSHOT"
  :description "Example using instaparse"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [instaparse "1.4.3"]
                 [rhizome "0.2.7"]]
  :main ^:skip-aot example.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
