#!/bin/bash
#

set -o errexit
set -o xtrace

http :3000/country?_limit=10
http :3000/country/999
http :3000/country/191
http :3000/country?code2=HR
http :3000/country?q=us

http :3000/country id=999 code2=FK code3=FAK name=FAKE
http :3000/country/999

exit 0
