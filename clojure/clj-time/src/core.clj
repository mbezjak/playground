(ns core
  (:require
   [clj-time.core :as time]
   [clj-time.format :as time-format]
   [clojure.set :as set]))

(time-format/parse-local-date "2024-04-24T00:00:00.000Z")
(time-format/parse-local-date "2024-04-23T23:59:59.000Z")
time-format/formatters
(time-format/show-formatters)
(time/now)
(java.util.Date.)
(System/getProperty "user.timezone")
(java.util.TimeZone/getDefault)
(doseq [formatter-name (map #(get (set/map-invert time-format/formatters) %)
                            (vals time-format/formatters))]
  (try
    (println "Succeeded" formatter-name
             (time-format/parse-local-date (get time-format/formatters formatter-name)
                                           "2024-04-24T00:00:00.000Z"))
    (catch Exception e
      (println "Failed " formatter-name))))
