(ns rfview.db)

(def default-db
  {:headers {:id "ID"
             :naziv "Naziv"}
   :data [{:id "004" :naziv "AFGANISTAN"}
          {:id "040" :naziv "AUSTRIJA"}
          {:id "076" :naziv "BRAZIL"}
          {:id "191" :naziv "HRVATSKA"}]
   :render {:type :grid}})

(defn find-row [db id]
  (let [data (:data db)]
    (some #(when (= id (:id %)) %) data)))

(comment
  (find-row default-db "191")
  )
