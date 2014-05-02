#!/bin/bash
#

set -o errexit

declare -r base='http://localhost:5050'

function ping  { curl $base/ping; }
function names { curl $base/book/names; }
function get   { curl $base/book/$1; }
function post  { curl -X POST -d "$1" $base/book; }
function newline { echo; }
function run {
    echo -n "$@ = "
    "$@"
    newline
}


run ping
run names
run post "name=foo&pages=512"
run post "name=bar&pages=128"
run post "name=qux&pages=64"
run names
run get foo
run get bar
run get qux
run get none

exit 0
