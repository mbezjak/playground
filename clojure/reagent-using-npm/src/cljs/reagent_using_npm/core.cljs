(ns reagent-using-npm.core
  (:require [reagent.core :as reagent :refer [atom]]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            react-simpletabs))

;; -------------------------
;; Views

(defn home-page []
  [:div [:h2 "Welcome to reagent-using-npm"]
   [:div [:a {:href "/about"} "go to about page"]]])

(defn about-page []
  [:div [:h2 "About reagent-using-npm"]
   [:div [:a {:href "/"} "go to the home page"]]])

;; -------------------------
;; Routes

(defonce page (atom #'home-page))

(defn panel-1 []
  [(reagent/adapt-react-class react-simpletabs/Panel)
   {:title "Tab #1"}
   [:h2 "Hello from 1"]])

(defn panel-2 []
  [(reagent/adapt-react-class react-simpletabs/Panel)
   {:title "Tab #2"}
   [:h2 "Hello from 2"]])

(defn tabs []
  [(reagent/adapt-react-class react-simpletabs)
   {}
   [panel-1]
   [panel-2]])

(defn current-page []
  [:div [tabs]])

(secretary/defroute "/" []
  (reset! page #'home-page))

(secretary/defroute "/about" []
  (reset! page #'about-page))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
