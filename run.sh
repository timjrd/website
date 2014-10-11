#!/bin/bash

ghc --make -static -optl-static Main.hs

mkdir -p .data-dir/static
ln -sr style.css face.jpg .data-dir/static

echo -n '\n:: starting the server (127.0.0.1:8000) ... ' >&2
./Main 127.0.0.1 8000 .data-dir




