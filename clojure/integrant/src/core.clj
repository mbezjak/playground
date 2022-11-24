(ns core)

;; https://github.com/weavejester/integrant
(require '[integrant.core :as ig])

(def config
  {:adapter/jetty {:port 8080 :handler (ig/ref :handler/greet)}
   :handler/greet {:name "Alice"}})
(def config-string "
  {:adapter/jetty {:port 8080 :handler #ig/ref :handler/greet}
   :handler/greet {:name \"Alice\"}}
")
(def config (ig/read-string config-string))

(require '[ring.adapter.jetty :as jetty]
         '[ring.util.response :as resp])

(defmethod ig/init-key :adapter/jetty [_ {:keys [handler] :as opts}]
  (jetty/run-jetty handler (-> opts (dissoc :handler) (assoc :join? false))))

(defmethod ig/init-key :handler/greet [_ {:keys [name]}]
  (fn [_] (resp/response (str "Hello " name))))

(defmethod ig/halt-key! :adapter/jetty [_ server]
  (.stop server))

(def system (ig/init config))
(def system (ig/init config [:adapter/jetty]))
((:handler/greet system) nil)

(ig/halt! system)
(meta system)



(defmethod ig/init-key :adapter/jetty [_ opts]
  (let [handler (atom (delay (:handler opts)))
        options (-> opts (dissoc :handler) (assoc :join? false))]
    {:handler handler
     :server (jetty/run-jetty (fn [req] (@@handler req)) options)}))

(defmethod ig/halt-key! :adapter/jetty [_ {:keys [server]}]
  (.stop server))

(defmethod ig/suspend-key! :adapter/jetty [_ {:keys [handler]}]
  (reset! handler (promise)))

(defmethod ig/resume-key :adapter/jetty [key opts old-opts old-impl]
  (if (= (dissoc opts :handler) (dissoc old-opts :handler))
    (do (deliver @(:handler old-impl) (:handler opts))
        old-impl)
    (do (ig/halt-key! key old-impl)
        (ig/init-key key opts))))

(def system (ig/init config))
(ig/suspend! system)
(def system (ig/resume config system))



(derive :adapter/jetty :adapter/ring)
(def system (ig/init config [:adapter/ring]))
(def system (ig/init config [:adapter/jetty]))
(ig/halt! system)

(derive :example/web-1 :adapter/jetty)
(derive :example/web-2 :adapter/jetty)
(def config
  {:example/web-1 {:port 8080 :handler (ig/ref :handler/greet)}
   :example/web-2 {:port 8081 :handler (ig/ref :handler/greet)}
   :handler/greet {:name "Alice"}})

(def config
  {[:adapter/jetty :example/web-1] {:port 8080 :handler (ig/ref :handler/greet)}
   [:adapter/jetty :example/web-2] {:port 8081 :handler (ig/ref :handler/greet)}
   :handler/greet {:name "Alice"}})


(require '[clojure.spec.alpha :as s])
*clojure-version*
(s/def ::port pos-int?)
(s/def ::handler fn?)

(defmethod ig/pre-init-spec :adapter/jetty [_]
  (s/keys :req-un [::port ::handler]))

(s/def ::name string?)

(defmethod ig/pre-init-spec :handler/greet [_]
  (s/keys :req-un [::name]))
