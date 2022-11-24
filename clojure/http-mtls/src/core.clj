(ns core)

;; https://github.com/dakrone/clj-http/issues/252
;; https://github.com/dakrone/clj-http/blob/a565546284351c18acee9533c8ceb34c43d1750c/test/clj_http/test/conn_mgr_test.clj#L66
;; http://ring-clojure.github.io/ring/ring.adapter.jetty.html
;; http://coryplusplus.com/2017/Java-Mutual-Authentication/
(require '[ring.adapter.jetty :as jetty])
(require '[clj-http.client :as client])

;; setup:
;; $ mkdir /tmp/foo
;; $ cd /tmp/foo
;; $ step-ca `step path`/config/ca.json
;; $ step ca root root.crt
;; $ step ca certificate localhost server.crt server.key
;; $ step ca certificate mbezjak client.crt client.key
;; $ openssl pkcs12 -export -in server.crt -inkey server.key -out server.p12 -name server -CAfile root.crt
;; $ openssl pkcs12 -export -in client.crt -inkey client.key -out client.p12 -name client -CAfile root.crt
;; $ keytool -importkeystore -deststorepass keykey -destkeypass keykey -destkeystore server-keystore -deststoretype PKCS12 -srcstorepass keykey -srckeystore server.p12 -srcstoretype PKCS12 -alias server
;; $ keytool -import -alias root -keystore server-keystore -file root.crt

(defn handler [req]
  (let [cert (:ssl-client-cert req)]
    (println :ssl-client-cert cert)
    {:status (if cert 200 403)}))
(def server
  (jetty/run-jetty
   handler
   {:port 8080
    :ssl? true
    :ssl-port 8443
    :host "localhost"
    :join? false
    :client-auth :want
    :keystore "/tmp/foo/server-keystore"
    :key-password "keykey"}))
(.stop server)

;; test:
;; $ curl --cacert root.crt --cert client.crt --key client.key -v https://localhost:8443/

;; then use clj-http
(client/request {:url "https://localhost:8443/"})

(client/request {:request-method :get
                 :scheme :https
                 :uri "/"
                 :server-port 8443
                 :server-name "localhost"})

(client/request {:request-method :get
                 :scheme :https
                 :uri "/"
                 :server-port 8443
                 :server-name "localhost"
                 :trust-store "/tmp/foo/server-keystore"
                 :trust-store-type "PKCS12"
                 :trust-store-pass "keykey"})

(client/request {:request-method :get
                 :scheme :https
                 :uri "/"
                 :server-port 8443
                 :server-name "localhost"
                 :trust-store "/tmp/foo/server-keystore"
                 :trust-store-type "PKCS12"
                 :trust-store-pass "keykey"
                 :keystore "/tmp/foo/client.p12"
                 :keystore-type "PKCS12"
                 :keystore-pass "keykey"})
