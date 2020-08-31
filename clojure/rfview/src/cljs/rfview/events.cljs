(ns rfview.events
  (:require
   [re-frame.core :as re-frame]
   [rfview.db :as db]))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(re-frame/reg-event-db
 ::show-details
 (fn [db [_ id]]
   (assoc db :render {:type :form :id id})))

(re-frame/reg-event-db
 ::show-grid
 (fn [db _]
   (assoc db :render {:type :grid})))

(re-frame/reg-event-db
 ::filter
 (fn [db [_ path value]]
   (assoc-in db [:render :filter] {:path path
                                   :value value})))
