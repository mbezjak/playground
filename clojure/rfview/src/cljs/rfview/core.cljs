(ns rfview.core
  (:require
   [reagent.core :as reagent]
   [reagent.dom :as rdom]
   [re-frame.core :as re-frame]
   [rfview.events :as events]
   [rfview.views :as views]
   [rfview.config :as config]))


(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (re-frame/clear-subscription-cache!)
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [views/app] root-el)))

(defn init []
  (re-frame/dispatch-sync [::events/initialize-db])
  (dev-setup)
  (mount-root))

(comment
  (deref re-frame.db/app-db)
  (re-frame/dispatch [:rfview.events/initialize-db])
  (re-frame/dispatch [:rfview.events/show-grid])
  (re-frame/dispatch [:rfview.events/show-details "040"]))
