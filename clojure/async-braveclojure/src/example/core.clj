(ns example.core
  (:require [clojure.core.async :as async :refer [>! <! >!! <!! go chan buffer close! thread alts! alts!! timeout]]))

(defn echo-simple []
  (let [c (chan)]
    (go (println (<! c)))
    (>!! c "ketchup")))

(defn echo-buffer []
  (let [c (chan 2)]
    (>!! c "ketchup")
    (>!! c "ketchup!")
    (go (println (<! c)))
    (>!! c "ketchup!!")))

(defn -main [& args]
  (echo-simple)
  (echo-buffer))
