(ns core
  (:require [clara.rules :refer :all]
            [clara.tools.inspect :as inspect]))

(defrecord SupportRequest [client level])

(defrecord HasHighSupportRequest [client])

(defrecord ClientRepresentative [name client])

(defrecord Notify [name client])

(defrule is-important
  [SupportRequest (= :high level) (= ?client client)]
  =>
  (insert! (->HasHighSupportRequest ?client)))

(defrule notify-important-client-rep
  [HasHighSupportRequest (= ?client client)]
  [ClientRepresentative (= ?client client) (= ?name name)]
  =>
  (insert! (->Notify ?name ?client)))

(defrule println-notify
  [Notify (= ?name name) (= ?client client)]
  =>
  (println "Notify" ?name "that" ?client "has a new support request!"))

(def s
  (-> (mk-session)
      (insert (->ClientRepresentative "Alice" "Acme")
              (->SupportRequest "Acme" :high))
      (fire-rules)))

(inspect/inspect s)
(inspect/explain-activations s)
