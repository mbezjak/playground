(ns core)

(require '[martian.core :as martian]
         '[martian.clj-http :as martian-http]
         '[cheshire.core :as cheshire])

(def m (martian-http/bootstrap-openapi "https://pedestal-api.herokuapp.com/swagger.json"))
(martian/explore m)
(martian/response-for m :all-pets)

(def m (martian-http/bootstrap-openapi "http://localhost:8000/swagger.json"))
(martian/explore m)
(martian/response-for m :create-support-ticket)
