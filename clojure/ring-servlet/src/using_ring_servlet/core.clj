(ns using-ring-servlet.core
  (:require ring.util.servlet)
  (:gen-class
   :name test.ring.servlet.Listener
   :implements [javax.servlet.ServletContextListener]
   :main false))

(defn handler [req]
  {:status 200
   :body "works!"})

(def servlet (ring.util.servlet/servlet handler))

(defn -contextInitialized [this event]
  (println "Context initialized")
  (let [context (.getServletContext event)]
    (println "Context is" context)
    (doto (.addServlet context "foo" servlet)
      (.addMapping (into-array String ["/"])))
    (println "Done registering")))

(defn -contextDestroyed [this event])
