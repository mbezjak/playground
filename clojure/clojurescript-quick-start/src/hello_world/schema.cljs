(ns hello-world.schema
  (:require [schema.core :as s :include-macros true]))

(def Data {:a {:b s/Str :c s/Int}})

(s/validate Data {:a {:b "Hello" :c 42}})
