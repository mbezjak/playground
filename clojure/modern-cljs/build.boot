(set-env!
 :source-paths #{"src/cljs"}
 :resource-paths #{"html"}

 :dependencies '[[adzerk/boot-cljs "1.7.228-1" :scope "test"]])

(require '[adzerk.boot-cljs :refer [cljs]])
