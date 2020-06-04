(ns rfview.subs
  (:require
   [re-frame.core :as re-frame]
   [rfview.db :as db]))

(re-frame/reg-sub
 ::data
 (fn [db]
   (:data db)))

(re-frame/reg-sub
 ::headers
 (fn [db]
   (:headers db)))

(re-frame/reg-sub
 ::render-type
 (fn [db]
   (get-in db [:render :type])))

(re-frame/reg-sub
 ::render-id
 (fn [db]
   (get-in db [:render :id])))

(re-frame/reg-sub
 ::row
 (fn [db [_ id]]
   (db/find-row db id)))
