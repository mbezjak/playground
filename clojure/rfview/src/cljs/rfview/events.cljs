(ns rfview.events
  (:require
   [re-frame.core :as re-frame]
   [rfview.db :as db]))

(re-frame/reg-event-db
 ::initialize-db
 (fn [_ _]
   db/default-db))

(defn vec-remove
  "remove elem in coll"
  [pos coll]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(re-frame/reg-event-db
 ::delete-item
 (fn [db [_ index]]
   {:items (vec-remove index (:items db))}))
