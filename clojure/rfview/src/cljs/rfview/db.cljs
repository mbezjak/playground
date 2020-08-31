(ns rfview.db
  (:require [clojure.string :as string]))

(def default-db
  {:headers [{:path :id :name "ID"}
             {:path :naziv :name "Naziv"}]
   :data [{:id "004" :naziv "AFGANISTAN"}
          {:id "040" :naziv "AUSTRIJA"}
          {:id "076" :naziv "BRAZIL"}
          {:id "191" :naziv "HRVATSKA"}]
   :render {:type :grid}})

(defn find-row [db id]
  (let [data (:data db)]
    (some #(when (= id (:id %)) %) data)))

(defn find-header [db id]
  (let [headers (:headers db)]
    (some #(when (= id (:path %)) %) headers)))

(defn filter-data [db {:keys [path value]}]
  (filter (fn [row]
            (let [rv (get row path)]
              (or (not rv) (string/starts-with? (string/lower-case rv) value))))
          (:data db)))

(comment
  (find-row default-db "191")
  (find-header default-db :id)

  (filter-data default-db {:path :naziv :value "afg"})
  (filter-data default-db {:path :naziv :value "a"})
  (filter-data default-db nil)
  (filter-data default-db {:path :naziv :value ""})
  )
