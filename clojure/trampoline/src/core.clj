(ns core)

(require '[clj-http.client :as client])

(client/get "http://ifconfig.me")

(defn unstable-request []
  (let [r (Math/random)]
    (if (< r 0.8)
      (throw (ex-info "Cannot request" {:random r}))
      (client/get "http://ifconfig.me"))))
(unstable-request)

(defn retry-forever* [f]
  (try
    (reduced (f))
    (catch Exception e
      (println "Got an error (" (-> e ex-data :random) "), retrying.")
      #(retry-forever* f))))

(defn retry-forever [f]
  (unreduced (trampoline #(retry-forever* f))))

(retry-forever unstable-request)

(fn? (reduced {:a 1}))
