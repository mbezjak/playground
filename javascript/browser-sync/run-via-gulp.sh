#!/bin/bash
#

set -o errexit

[[ -d node_modules/gulp ]] || npm install browser-sync gulp --save-dev
node_modules/.bin/gulp serve

exit 0
