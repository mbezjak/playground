(ns core)

;; https://github.com/Factual/durable-queue
(require '[durable-queue :as q])

(def q (q/queues "/tmp" {}))
(q/put! q :foo "a task")
(q/take! q :foo 10 :timed-out!)

(q/stats q)
