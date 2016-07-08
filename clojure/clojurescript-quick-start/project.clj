(defproject hello_world "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.9.89"]
                 [cljs-ajax "0.3.10"]]
  :plugins [[lein-cljsbuild "1.1.3"]]
  :cljsbuild {:builds
              [{:source-paths ["src_lein"]
                :compiler {:output-to "out/main.js"
                           :optimizations :advanced}}]})
