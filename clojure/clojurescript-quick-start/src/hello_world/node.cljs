(ns hello-world.node
  (:require [cljs.nodejs :as nodejs]))

(nodejs/enable-util-print!)

(defn -main [& args]
  (ffirst [1])
  (println "Hello world!"))

(set! *main-cli-fn* -main)
