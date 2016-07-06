(ns hello-world.core
  (:require cljsjs.react))

(enable-console-print!)

(println "Hello world!" js/React.version)
