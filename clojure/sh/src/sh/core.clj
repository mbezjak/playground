(ns sh.core)

(defn succesful [cmd & args]
  (let [{exit :exit out :out} (apply clojure.java.shell/sh cmd args)]
    (assert (= exit 0) "Not successful")
    out))

(defn ls [& args]
  (apply succesful "ls" args))

(defn stat [& args]
  (apply succesful "stat" args))


(defn ls-tmp []
  (ls "-al" "/tmp"))

(defn stat-tmp []
  (stat "/tmp"))

(defn run []
  (println (ls-tmp))
  (println (stat-tmp)))
