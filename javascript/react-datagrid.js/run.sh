#!/bin/bash
#

set -o errexit

[[ -d node_modules ]] || npm install
npm run build
xdg-open index.html

exit 0
