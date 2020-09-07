(ns rfview.views
  (:require
   [re-frame.core :as re-frame]
   [rfview.subs :as subs]
   [rfview.events :as events]))

(defn render-row [id row]
  ^{:key id}
  [:tr {:on-click #(re-frame/dispatch [::events/show-details id])}
   (map (fn [col] ^{:key col} [:td col]) row)])

(defn render-header [{:keys [path name filter-value]}]
  ^{:key name}
  [:th
   [:p name]
   [:input {:type "textfield"
            :on-change (fn [e]
                         (re-frame/dispatch [::events/filter path (-> e .-target .-value)]))
            :value filter-value}]])

(defn grid []
  (let [{:keys [headers rows]} @(re-frame/subscribe [::subs/table])]
    [:table
     [:thead
      [:tr
       (map render-header headers)]]
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

(defn navigation []
  [:div.navigation
   [:b "Navigation"]
   [:ul
    [:li [:a {:href "#" :on-click #(re-frame/dispatch [::events/show-grid])}
          "Countries"]]]])

(defn app []
  [:div.app
   [navigation]
   [main-panel]])
