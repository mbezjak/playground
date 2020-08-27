(ns rfview.views
  (:require
   [re-frame.core :as re-frame]
   [rfview.subs :as subs]
   [rfview.events :as events]))

(defn render-row [id row]
  ^{:key id}
  [:tr {:on-click #(re-frame/dispatch [::events/show-details id])}
   (map (fn [col] [:td col]) row)])

(defn grid []
  (let [{:keys [headers rows]} @(re-frame/subscribe [::subs/table])]
    [:table
     [:thead
      [:tr
       (map (fn [h] [:th h]) headers)]]
     [:tbody
      (map (fn [row] (render-row (first row) row)) rows)]]))

(defn render-field [{:keys [label name value]}]
  [:label
   [:span label]
   [:input {:type "textfield" :name name :read-only true :value value}]])

(defn form [id]
  (let [fields @(re-frame/subscribe [::subs/fields id])]
    [:div
     [:button.back
      {:on-click #(re-frame/dispatch [::events/show-grid])}
      "back"]
     [:form
      (map render-field fields)]]))

(defn main-panel []
  (let [type @(re-frame/subscribe [::subs/render-type])]
    (case type
      :grid [grid]
      :form [form @(re-frame/subscribe [::subs/render-id])])))
