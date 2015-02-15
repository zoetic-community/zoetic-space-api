#!/usr/bin/env bash

PID=`lsof -i4TCP:3000 |grep zoetic |awk -F ' ' '{print $2}'`
echo $PID

while kill -0 $PID >/dev/null 2>&1
do
  kill -TERM $PID
done

set -e
cabal build
./dist/build/zoetic-space-api/zoetic-space-api
