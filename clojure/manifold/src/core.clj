(ns core)

;; https://github.com/ztellman/manifold
(require '[manifold.deferred :as d])
(def x (d/deferred))
(d/deferred? x)
(d/deferred? 1)
(d/success! x :foo)
(d/error! x :foo)
(d/error! x {:a 1 :b 2})
(d/error! x (Exception. "there"))
(d/on-realized x
               (fn [a] (println "success" a))
               (fn [a] (println "error" a)))
@x

(require '[manifold.stream :as s])
(def x (s/stream))
(s/put! x 1)
(deref (s/take! x ::drained))

(def x (d/deferred))
(d/chain x inc inc inc #(println "x + 3 =" %))
(d/on-realized (d/chain x inc inc inc)
               (fn [a] (println "x + 3 =" a))
               (fn [a] (println "error" a)))
(d/chain x
         #(future (inc %))
         #(println "the future returned" %))
(-> x
    (d/chain dec #(/ 1 %))
    (d/catch Exception #(println "whoops, that didn't work:" %)))
(d/success! x 0)
(d/success! x 1)

(d/chain 1 inc)

@(d/zip (future 1) (future 2) (future 3))
@(d/timeout!
  (d/future (Thread/sleep 1000) :foo)
  100
  :bar)

@(d/chain
  (d/timeout! (d/future (Thread/sleep 1000) :foo) 100 :bar)
  str)

@(d/timeout!
  (d/chain (d/future (Thread/sleep 1000) :foo) str)
  100
  :bar)

(def x (d/deferred))
(deliver x :foo)

(defn call-service-a []
  (d/future 1))
(defn call-service-b []
  (d/future 2))

(let [a (call-service-a)
      b (call-service-b)]
  (d/chain (d/zip a b)
           (fn [[a b]]
             (+ a b))))

(d/let-flow [a (call-service-a)
             b (call-service-b)]
  (+ a b))

@(let [a (d/future 1)]
  (d/let-flow [b (d/future (+ a 1))
               c (+ b 1)]
    (+ c 1)))


(require '[manifold.stream :as s])
(def s (s/stream 2))
(s/put! s 1)
@(s/try-put! s :foo 1000 ::timeout)
(s/take! s)
(s/take! s ::drained)
@(s/try-take! s ::drained 1000 ::timeout)
(s/close! s)
(s/closed? s)
(s/drained? s)

(s/description s)
(s/stream->seq s)
(s/consume #(prn 'message! %) s)
(s/->source :foo :x)
(->> [1 2 3]
     s/->source
     (s/map inc)
     s/stream->seq)
(->> [1 2 3]
     (s/map inc)
     s/stream->seq)

(def s (s/stream))
(def a (s/map inc s))
(def b (s/map dec s))
@(s/put! s 0)
@(s/take! a)
@(s/take! b)

(def a (s/stream))
(def b (s/stream))
(s/connect a b {:description "a connection"})
(s/connect-via a #(s/put! b (inc %)) b)
@(s/put! a 1)
@(s/take! b)
(s/close! a)
(s/close! b)
(s/description a)
(s/description b)
(s/downstream a)
(s/map inc a)

(def a (s/stream))
(def b (s/buffer 2 a))
(def b (s/buffer count 2 a))
(def a (s/stream 2))
(def b (s/throttle 1 a))
(s/put! a 1)
(s/put! a 2)
(s/put! a [1 2 3])
(s/put-all! a [1 2 3])
(s/close! a)
(s/description a)
(s/description b)
(s/downstream a)
@(s/take! b)
(s/stream->seq b)

(require '[manifold.bus :as b])
(def a (b/event-bus))
(def s (b/subscribe a :foo))
(b/publish! a :foo 1)
(s/take! s)
(b/topic->subscribers a)

(require '[manifold.executor :as e])
(e/register-wait-pool-stats-callback println)
(d/chain (future 1) inc)
(bean (.getStats (e/wait-pool)))

(def executor (e/fixed-thread-executor 42))
(-> (d/future 1)
    (d/onto executor)
    (d/chain inc inc inc))
