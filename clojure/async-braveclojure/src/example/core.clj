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

(defn hi-chan []
  (let [c (chan)]
    (doseq [n (range 1000)]
      (go (>! c (str "hi " n))))))

(defn thread-put []
  (let [c (chan)]
    (thread (println (<!! c)))
    (>!! c "mustard")))

(defn thread-get []
  (let [t (thread "chili")]
    (<!! t)))

(defn hot-dog-machine []
  (let [in (chan)
        out (chan)]
    (go (<! in)
        (>! out "hot dog"))
    [in out]))

(defn use-hot-dog-machine []
  (let [[in out] (hot-dog-machine)]
    (>!! in "pocket lint")
    (<!! out)))

(defn hot-dog-machine-v2 [hot-dog-count]
  (let [in (chan)
        out (chan)]
    (go (loop [hc hot-dog-count]
          (if (> hc 0)
            (let [input (<! in)]
              (if (= input 3)
                (do (>! out "hot dog")
                    (recur (dec hc)))
                (do (>! out "wilted lettuce")
                    (recur hc))))
            (do (close! in)
                (close! out)))))
    [in out]))

(defn use-hot-dog-machine-v2 []
  (let [[in out] (hot-dog-machine-v2 2)]
    (>!! in "pocket lint")
    (println (<!! out))
    (>!! in 3)
    (println (<!! out))
    (>!! in 3)
    (println (<!! out))
    (println (>!! in 3))
    (println (<!! out))))

(defn -main [& args]
  (echo-simple)
  (echo-buffer)
  (hi-chan)
  (thread-put)
  (thread-get)
  (use-hot-dog-machine)
  (use-hot-dog-machine-v2))
