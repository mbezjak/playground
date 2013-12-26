;; see https://github.com/clojure/tools.cli

(ns cmdargs.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as string])
  (:gen-class))

(def cli-options
  [["-a" "--left NUM" "Left argument"
    :default 0
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 100) "Must be a number: [0-100]"]]
   ["-b" "--right NUM" "Right argument"
    :default 0
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 100) "Must be a number: [0-100]"]]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Trivial calculator"
        ""
        "Usage: program-name [options]"
        ""
        options-summary]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred:\n\n"
       (string/join \newline errors)))

(defn print-options [options]
  (println (str "Options: " options)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]

    (cond
     (:help options) (exit 0 (usage summary))
     errors (exit 1 (error-msg errors)))

    (print-options options)

    (let [{:keys [left right]} options]
      (println (+ left right)))))

;; try
;; $ lein uberjar
;; $ jar -jar target/cmdargs-0.1.0-SNAPSHOT-standalone.jar --help
