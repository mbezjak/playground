(ns core)

;; http://ring-clojure.github.io/ring/ring.adapter.jetty.html
(require '[ring.adapter.jetty :as jetty])

;; setup:
;; $ cd /tmp
;; $ mkcert -pkcs12 localhost

(defn handler [req]
  {:status 200
   :body "hello"})

(def server
  (jetty/run-jetty
   handler
   {:port 8080
    :ssl? true
    :ssl-port 8443
    :host "localhost"
    :join? false
    :keystore "/tmp/localhost.p12"
    :key-password "changeit"}))

(.stop server)
