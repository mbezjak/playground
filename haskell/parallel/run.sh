#!/bin/bash
#

set -o errexit

ghc -O2 --make maths.hs -threaded -rtsopts
time ./maths +RTS -N

exit 0
