(ns component.core
  (:require [com.stuartsierra.component :as component])
  (:import [java.sql DriverManager]))

(defn connect-to-database [host port db server username password]
  (let [tpl "jdbc:informix-sqli://%s:%s/%s:informixserver=%s;CLIENT_LOCALE=cs_cz.CP1250;DB_LOCALE=cs_cz.CP1250"
        url (format tpl host port db server)]
    (DriverManager/getConnection url username password)))

(defn execute-query [conn query]
  (let [stmt (.createStatement conn)
        rs (.executeQuery stmt query)
        result (doall (resultset-seq rs))]
    (.close stmt)
    result))

(defrecord Database [host port db server username password connection]
  component/Lifecycle

  (start [component]
    (println ";; Starting database")
    (let [conn (connect-to-database host port db server username password)]
      (assoc component :connection conn)))

  (stop [component]
    (println ";; Stopping database")
    (.close (:connection component))
    (assoc component :connection nil)))

(defn get-user [database]
  (first (execute-query (:connection database) "SELECT FIRST 1 * from hx_operater")))

(defrecord FirstUserComponent [database]
  component/Lifecycle

  (start [this]
    (println ";; Starting FirstUserComponent")
    (assoc this :row (get-user database)))

  (stop [this]
    (println ";; Stopping FirstUserComponent")
    this))

(defn first-user-component []
  (map->FirstUserComponent {}))

(defrecord AppComponent [first-user]
  component/Lifecycle

  (start [this]
    (printf "Row is: %s\n" (:row first-user))
    this)
  (stop [this]
    this))

(defn example-system [cfg]
  (-> (component/system-map
       :database (map->Database cfg)
       :first-user (first-user-component)
       :app (->AppComponent {}))
      (component/system-using
       {:first-user [:database]
        :app [:first-user]})))

(defn -main [host port db server username password & args]
  (println "About to start everything")
  (component/start (example-system {:host host
                                    :port port
                                    :db db
                                    :server server
                                    :username username
                                    :password password})))
