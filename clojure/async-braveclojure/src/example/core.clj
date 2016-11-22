(ns example.core
  (:require [clojure.core.async :as async :refer [>! <! >!! <!! go chan buffer close! thread alts! alts!! timeout]]
            [clojure.string]))

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


(defn upload [headshot c]
  (go (Thread/sleep (rand 100))
      (>! c headshot)))

(defn upload-files []
  (let [c1 (chan)
        c2 (chan)
        c3 (chan)]
    (upload "serious.jpg" c1)
    (upload "fun.jpg" c2)
    (upload "sassy.jpg" c3)

    (let [[headshot channel] (alts!! [c1 c2 c3 (timeout 20)])]
      (if headshot
        (println "Sending headshot notification for" headshot)
        (println "Timed out!")))))


(defn append-to-file [filename s]
  (spit filename s :append true))

(defn format-quote [quote]
  (str "=== BEGIN QUOTE ===\n" quote "=== END QUOTE ===\n\n"))

(defn random-quote []
  (format-quote (slurp "http://www.braveclojure.com/random-quote")))

(defn snag-quotes [filename num-quotes]
  (let [c (chan)]
    (go (while true (append-to-file filename (<! c))))
    (dotimes [n num-quotes] (go (>! c (random-quote))))))


(defn upper-caser [in]
  (let [out (chan)]
    (go (while true (>! out (clojure.string/upper-case (<! in)))))
    out))

(defn reverser [in]
  (let [out (chan)]
    (go (while true (>! out (clojure.string/reverse (<! in)))))
    out))

(defn printer [in]
  (go (while true (println (<! in)))))

(defn callbacks-as-channels []
  (let [in-chan (chan)
        upper-caser-out (upper-caser in-chan)
        reverser-out (reverser upper-caser-out)]
    (printer reverser-out)

    (>!! in-chan "redrum")
    (>!! in-chan "!etorw ehs redrum")

    (>!! in-chan "repaid")))

(defn -main [& args]
  (echo-simple)
  (echo-buffer)
  (hi-chan)
  (thread-put)
  (thread-get)
  (use-hot-dog-machine)
  (use-hot-dog-machine-v2)
  (upload-files)
  (snag-quotes "/tmp/quote" 4)
  (callbacks-as-channels))
