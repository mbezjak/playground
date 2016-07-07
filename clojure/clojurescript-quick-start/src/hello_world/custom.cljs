(ns hello-world.custom)

(enable-console-print!)

(let [yay (js/yayQuery)]
  (.sayHello yay (.getMessage yay)))
