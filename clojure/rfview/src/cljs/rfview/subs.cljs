(ns rfview.subs
  (:require
   [re-frame.core :as re-frame]))

(re-frame/reg-sub
 ::items
 (fn [db]
   (:items db)))
