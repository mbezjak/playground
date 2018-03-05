(defproject using-ring-servlet "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [javax.servlet/javax.servlet-api "3.1.0"]
                 [ring/ring-servlet "1.6.3"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
