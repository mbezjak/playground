(ns rfview.views
  (:require
   [re-frame.core :as re-frame]
   [rfview.subs :as subs]
   [rfview.events :as events]))

(defn render-item [todo]
  ^{:key todo} [:li todo])

(defn main-panel []
  (let [items (re-frame/subscribe [::subs/items])]
    [:div
     [:ul
      (map render-item @items)]
     [:button
      {:on-click #(re-frame/dispatch [::events/delete-item 0])}
      "Delete 1st"]]))

(comment
  (re-frame/dispatch [::events/delete-item 1])
  (re-frame/dispatch [::events/initialize-db])
  )
