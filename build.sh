#!/bin/bash

# install dependencies
sudo apt-get update
sudo apt-get install -y g++ cmake autoconf libgmp-dev libmpfr-dev libsqlite3-dev m4 pkg-config zlib1g-dev gcc make software-properties-common

sudo add-apt-repository ppa:avsm/ppa
sudo apt-get update
sudo apt-get install -y opam

# init submodule
git submodule init
git submodule update

# build infer
pushd tracer-infer
NINJA=no ./build-infer.sh clang -y
popd

# build ranking system
eval $(opam env)
pushd rank
make
popd
