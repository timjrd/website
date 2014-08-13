#!/bin/bash

ghc --make Main.hs

mkdir -p .data-dir/static
ln -sr style.css face.jpg .data-dir/static

export OPENSHIFT_DATA_DIR=.data-dir
echo -n 'starting the server (127.0.0.1:8000) ... ' >&2
./Main 127.0.0.1 8000




