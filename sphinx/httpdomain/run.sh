#!/bin/bash
#

set -o errexit

rm -rf build
sphinx-build2 -d build/doctrees -b html . build/html

exit 0
