(ns example.core
  (:require [clojure.core.async :as async]))

(defn -main [& args]
  (def a-channel (async/chan 1))
  (async/>!! a-channel "Hello, World!")
  (println (async/<!! a-channel)))
