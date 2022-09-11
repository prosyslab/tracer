#!/bin/bash

# build infer
pushd tracer-infer
NINJA=no ./build-infer.sh clang -y
popd

# build ranking system
pushd rank
make
popd
