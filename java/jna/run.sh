#!/bin/bash
#

set -o errexit

gcc -o libctest.so -shared -fPIC ctest.c
javac -classpath jna.jar HelloWorld.java
LD_LIBRARY_PATH="$PWD" java -classpath jna.jar:. HelloWorld

exit 0
