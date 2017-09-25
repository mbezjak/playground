(defproject component "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [com.stuartsierra/component "0.3.2"]
                 [com.informix/ifxjdbc "4.10.jc6"]]
  :main ^:skip-aot component.core
  :target-path "target/%s"
  :repositories [["helix" "http://maven/repo"]])
