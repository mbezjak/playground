#!/bin/bash
#

set -o errexit

npm install
wget https://raw.githubusercontent.com/leebyron/jasmine-check/master/jasmine-check.js -O node_modules/jasmine-check/jasmine-check.js
npm run setup
xdg-open index.html

exit 0
