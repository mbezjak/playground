(ns clojurescript-using-npm.core
  (:require left-pad
            react
            react-dom))

(enable-console-print!)
(println "Left padding: " (left-pad "foo" 10 "x"))

(def title
  (react/createElement "div" nil
             (react/createElement "h1" nil "Page title")))

(defn on-js-reload []
  (react-dom/render title (.getElementById js/document "app")))
