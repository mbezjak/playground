#!/bin/bash
#

set -o errexit
set -o pipefail

rm -rf rootfs
mkdir rootfs
docker export $(docker create busybox) | tar -C rootfs -xf -

gcc isolate.c -o isolate
sudo ./isolate bin/sh
echo "Exited from isolated process"

exit 0
