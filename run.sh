#!/bin/bash

echo ':: compiling...'
make

mkdir -p .data-dir
ln -srT static .data-dir/static
killall SERVER
sleep 0.5

echo -e '\n:: (re)starting the server (127.0.0.1:8000)'
Builds/SERVER 127.0.0.1 8000 .data-dir &




