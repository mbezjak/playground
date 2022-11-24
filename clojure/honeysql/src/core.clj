(ns core)

;; https://github.com/jkk/honeysql
(require '[honeysql.core :as sql])
(require '[honeysql.format :as fmt])

(sql/format {:select [:a :b]})
(sql/format {:select [:a :b]
             :from [:foo]})
(sql/format {:select [:a :b]
             :from [:foo]
             :where [:= :f.a "baz"]})
(sql/format {:select [:a :b]
             :from [[:foo :f]]
             :where [:or
                     [:= :f.a "baz"]
                     [:= :f.b "bar"]]})
(sql/format {:select [:a :b]
             :offset 10
             :limit 10
             :from [:foo]
             :where [:= :f.a "baz"]})
(sql/format {:select [:*]
             :modifiers [:distinct]
             :from [:foo]})
(sql/format {:select [:a :b]
             :from [[:foo :f]]
             :left-join [:bar [:= :f.bar_id :b.id]]})

(sql/format {:select [:* (sql/call :current) (sql/raw "@x := 10")]})
(sql/format {:select [(sql/call :current :x)]})
(sql/format {:select [(sql/call :current :x.a)]})
(sql/format {:select [(sql/call :current :x-a)]})
(sql/format {:select [(sql/call :current "x")]})
(sql/format {:select [(sql/raw "MDY(1, 10, 2018)")]})

(sql/format {:insert-into :user_properties
             :columns [:id :name :value]
             :values [[1 "foo" "val1"]]})
(sql/format {:insert-into :user_properties
             :columns [:id :name :value]
             :values [[1 "foo" "val1"]
                      [2 "bar" "bar1"]]})
(sql/format {:insert-into :user_properties
             :values [{:id 1 :name "foo" :value "foo1"}]})
;; looking only at the first map!
(sql/format {:insert-into :user_properties
             :values [{:id 1 :name "foo" :value "foo1"}
                      {:id 2 :name "bar" :ts (sql/raw "CURRENT")}]})

(sql/format {:select [:*]
             :from [[:client :c]]
             :where [:in :c.country-id {:select [:id] :from [:country] :where [:= :code2 "HR"]}]})

(sql/format {:select [:%count.* :%max.id]
             :from [:foo]})
(sql/format {:select [:%count.*]
             :modifiers [:distinct]
             :from [:foo]})
(sql/format {:select [:%count-distinct.name]
             :from [:foo]})

"CASE WHEN broj_gresaka > 0 THEN 1 ELSE 0 END"
(sql/format {:select [(sql/call :case {:where [:> :broj-gresaka 0]} 1 :else 0)]})
(defmethod fmt/format-clause :inline-where [[_ pred] _]
  (fmt/format-predicate* pred))
(sql/format {:select [(sql/call :case {:inline-where [:> :broj-gresaka 0]} 1 :else 0)]})
(sql/format {:select [(sql/call :case {:inline-where [:> :broj-gresaka (sql/inline 0)]} #sql/inline 1 :else #sql/inline 0)]})

(sql/format {:select [#sql/call [:foo :a]]})
(sql/qualify :foo :bar :id)
