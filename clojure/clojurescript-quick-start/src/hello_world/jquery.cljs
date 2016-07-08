(ns hello-world.jquery
  (:require [cljsjs.jquery]))

(enable-console-print!)

(.text (js/$ "body") "Hello, World!")
