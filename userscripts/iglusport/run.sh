#!/bin/bash
#

set -o errexit
set -o pipefail

web-ext run --url "https://www.iglusport.hr"

exit 0
