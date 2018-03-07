(ns ^:figwheel-no-load reagent-using-npm.dev
  (:require
    [reagent-using-npm.core :as core]
    [devtools.core :as devtools]))

(devtools/install!)

(enable-console-print!)

(core/init!)
