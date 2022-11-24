(ns core)

(require '[clj-http.client :as client])
(require '[safely.core :refer [safely]])

(safely
    (client/get "https://google.com/not-found-12334")

    :on-error
    :max-retries 3
    :retry-delay [:random-exp-backoff :base 1000 :+/- 0.50])
