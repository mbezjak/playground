(ns core
  (:require [com.stuartsierra.frequencies :as freq]))

(defn example-sequence []
  (repeatedly 10000 #(rand-int 500)))

(example-sequence)

(def freq-map (frequencies (example-sequence)))
(freq/stats freq-map)
