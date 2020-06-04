(ns rfview.views
  (:require
   [re-frame.core :as re-frame]
   [rfview.subs :as subs]
   [rfview.events :as events]))

(defn render-row [row]
  ^{:key (:id row)}
  [:tr {:on-click #(re-frame/dispatch [::events/show-details (:id row)])}
   [:td (:id row)]
   [:td (:naziv row)]])

(defn grid []
  (let [data @(re-frame/subscribe [::subs/data])
        headers @(re-frame/subscribe [::subs/headers])]
    [:table
     [:thead
      [:tr
       [:th (:id headers)]
       [:th (:naziv headers)]]]
     [:tbody
      (map render-row data)]]))

(defn form [id]
  (let [row @(re-frame/subscribe [::subs/row id])]
    [:div
     [:button.back
      {:on-click #(re-frame/dispatch [::events/show-grid])}
      "back"]
     [:form
      [:label
       "ID: "
       [:input {:type "textfield" :name "id" :read-only true :value (:id row)}]]
      [:label
       "Naziv: "
       [:input {:type "textfield" :name "naziv" :read-only true :value (:naziv row)}]]]]))

(defn main-panel []
  (let [type @(re-frame/subscribe [::subs/render-type])]
    (case type
      :grid [grid]
      :form [form @(re-frame/subscribe [::subs/render-id])])))
