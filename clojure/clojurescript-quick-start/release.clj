(require 'cljs.build.api)

(cljs.build.api/build "src"
                      {:output-to "out/main.js"
                       :externs ["yayquery-externs.js"]
                       :optimizations :advanced})

(System/exit 0)
