(ns core)

;; https://github.com/ztellman/aleph
(require '[aleph.http :as http])

(defn handler [req]
  (prn req)
  {:status 200
   :headers {"content-type" "text/plain"}
   :body "hello!\n"})

(def server (http/start-server handler {:port 8080}))
(.close server)

(-> @(http/get "https://google.com")
    :body)

(require '[manifold.stream :as s])

(defn echo-handler [req]
  (let [s @(http/websocket-connection req)]
    (s/connect s s)))
(def server (http/start-server echo-handler {:port 8080}))
