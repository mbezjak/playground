(ns core)

(require '[muuntaja.core :as m])
(require '[muuntaja.middleware :as middleware])
(require '[clojure.edn :as edn])

(m/encode "application/json" {:a 1})
(slurp (m/encode "application/json" {:a 1}))

(->> {:a 1}
     (m/encode "application/json")
     (m/decode "application/json"))

(->> {:a 1/3}
     (m/encode "application/json")
     (m/decode "application/json"))

(->> {:a (java.util.Date.)}
     (m/encode "application/json")
     (m/decode "application/json"))

m/default-options

(defn echo [request]
  (def request request)
  {:status 200
   :body (:body-params request)})

(def app (middleware/wrap-format echo))


(def request
  {:headers {"content-type" "application/edn"
             "accept" "application/transit+json"}
   :body "{:a 1}"})

(edn/read-string (slurp (:body (app request))))
