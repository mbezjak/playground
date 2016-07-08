(ns hello-world.core
  (:require [ajax.core :refer [GET]]))

(GET "https://api.github.com/emojis")
