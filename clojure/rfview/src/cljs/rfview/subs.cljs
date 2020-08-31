(ns rfview.subs
  (:require
   [re-frame.core :as re-frame]
   [rfview.db :as db]))

(defn add-filter-value [h {:keys [path value]}]
  (if (= path (:path h))
    (assoc h :filter-value value)
    h))

(re-frame/reg-sub
 ::table
 (fn [db]
   (let [filter (-> db :render :filter)
         headers (:headers db)
         data (db/filter-data db filter)
         paths (map :path headers)]
     {:headers (map #(add-filter-value % filter) headers)
      :rows (for [row data]
              (for [p paths]
                (get row p)))})))

(re-frame/reg-sub
 ::render-type
 (fn [db]
   (get-in db [:render :type])))

(re-frame/reg-sub
 ::render-id
 (fn [db]
   (get-in db [:render :id])))

(re-frame/reg-sub
 ::fields
 (fn [db [_ id]]
   (let [row (db/find-row db id)]
     (for [[key value] row]
       {:label (:name (db/find-header db key))
        :name (name key)
        :value value}))))
