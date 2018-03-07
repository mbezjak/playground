(ns reagent-using-npm.prod
  (:require [reagent-using-npm.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
