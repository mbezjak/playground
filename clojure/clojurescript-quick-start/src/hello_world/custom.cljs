(ns hello-world.custom
  (:require [yq]))

(enable-console-print!)

(let [yay (js/yayQuery)]
  (.sayHello yay (.getMessage yay)))
