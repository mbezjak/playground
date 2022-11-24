(ns core)

(require '[net.cgrand.enlive-html :as html])

(def r (html/html-resource (java.net.URL. "https://www.helix.hr")))

(html/select r [:script])
