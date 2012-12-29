#!/bin/bash
#

set -o errexit

[[ ! -f compiled ]] && {
    (cd scala;  scalac  hello.scala)
    (cd groovy; groovyc hello.groovy)
    (cd java;   javac   hello.java)
    touch compiled
}

run() {
    echo -------
    time "$@"
    echo -------
    echo
}

run java -cp /usr/share/scala/lib/*:scala hello
run java -cp /usr/share/groovy/lib/*:groovy hello
run java -cp java hello
run clj clojure/hello.clj # uncompiled

exit 0
