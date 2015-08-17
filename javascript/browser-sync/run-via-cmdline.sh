#!/bin/bash
#

set -o errexit

[[ -d node_modules ]] || npm install browser-sync
node_modules/.bin/browser-sync start --server --files "*.js, *.css, *.html"

exit 0
