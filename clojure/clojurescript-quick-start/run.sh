#!/bin/bash
#

set -o errexit

# prerequisite: download the standalone ClojureScript JAR
# https://github.com/clojure/clojurescript/wiki/Quick-Start

declare -r file="${1:-build}"

if [[ $file =~ ^.*repl$ ]]; then
    rlwrap java -cp "cljs.jar:src" clojure.main "${file}.clj"
else
    java -cp "cljs.jar:src" clojure.main "${file}.clj"
fi


exit 0
