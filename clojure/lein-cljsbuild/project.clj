(defproject lein-cljsbuild-example "1.2.3"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.89"]]
  :plugins [[lein-cljsbuild "1.1.3"]]

  :cljsbuild
  { :builds [{:source-paths ["src"]
              :compiler {:output-to "target/main.js"
                         :optimizations :whitespace
                         :pretty-print true}}]})
