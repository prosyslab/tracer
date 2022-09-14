#!/bin/bash

# install dependencies
sudo apt-get update
sudo apt-get install -y sqlite3 curl opam g++ cmake autoconf libgmp-dev libmpfr-dev libsqlite3-dev m4 pkg-config zlib1g-dev gcc make

# init submodule
git submodule init
git submodule update

# build infer
pushd tracer-infer
NINJA=no ./build-infer.sh clang -y
popd

# build ranking system
eval $(opam env)
opam install ppx_deriving_yojson
pushd rank
make
popd
