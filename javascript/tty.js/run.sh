#!/bin/bash
#

set -o errexit

PYTHON=python2 npm install tty.js
node_modules/.bin/tty.js --config config.json

exit 0
