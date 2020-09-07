(ns rfview.db
  (:require [clojure.string :as string]))

(def default-db
  {:headers [{:path :id :name "ID"}
             {:path :naziv :name "Naziv"}]
   :data [{:id "004" :naziv "AFGANISTAN"}
          {:id "040" :naziv "AUSTRIJA"}
          {:id "076" :naziv "BRAZIL"}
          {:id "100" :naziv "BUGARSKA"}
          {:id "104" :naziv "MIJANMAR"}
          {:id "120" :naziv "KAMERUN"}
          {:id "124" :naziv "KANADA"}
          {:id "156" :naziv "KINA"}
          {:id "191" :naziv "HRVATSKA"}
          {:id "196" :naziv "CIPAR"}
          {:id "203" :naziv "ČEŠKA"}
          {:id "208" :naziv "DANSKA"}
          {:id "231" :naziv "ETIOPIJA"}
          {:id "232" :naziv "ERITREJA"}
          {:id "233" :naziv "ESTONIJA"}
          {:id "250" :naziv "FRANCUSKA"}
          {:id "266" :naziv "GABON"}
          {:id "268" :naziv "GRUZIJA"}
          {:id "410" :naziv "KOREJA, REPUBLIKA (JUŽNA KOREJA)"}
          {:id "422" :naziv "LIBANON"}
          {:id "426" :naziv "LESOTO"}
          {:id "428" :naziv "LETONIJA"}
          {:id "450" :naziv "MADAGASKAR"}
          {:id "454" :naziv "MALAVI"}
          {:id "458" :naziv "MALEZIJA"}
          {:id "462" :naziv "MALDIVI"}
          {:id "466" :naziv "MALI"}
          {:id "470" :naziv "MALTA"}
          {:id "686" :naziv "SENEGAL"}
          {:id "688" :naziv "SRBIJA"}
          {:id "690" :naziv "SEJŠELI"}
          {:id "694" :naziv "SIJERA LEONE"}
          {:id "752" :naziv "ŠVEDSKA"}
          {:id "756" :naziv "ŠVICARSKA"}
          {:id "807" :naziv "MAKEDONIJA"}
          {:id "818" :naziv "EGIPAT"}
          {:id "826" :naziv "VELIKA BRITANIJA"}]
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
