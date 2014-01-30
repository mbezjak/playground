(ns agent.core)

(defn write [writer msg]
  (.write writer (str msg "\n"))
  (.flush writer)
  writer)

(defn op [a msg]
  (fn [] (send-off a write msg)))

(defn run []
  (with-open [w (clojure.java.io/writer "file:///tmp/agent.txt" :append true)]
    (let [a (agent w)]
      (pcalls (op a "abc") (op a "def") (op a "ghi") (op a "jkl"))
      (op a "mno")
      (op a "prs")

      (Thread/sleep 1000)))

  ; not in clojure repl
  ;(shutdown-agents)

  (println (slurp "/tmp/agent.txt"))
  (clojure.java.io/delete-file "/tmp/agent.txt"))
