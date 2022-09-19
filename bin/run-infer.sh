#!/bin/bash

usage() {
  echo >&2 "Usage: ./$(basename "$0") -p PACKAGE -o OUT_DIR -t TIMEOUT -i \"INFER_OPTIONS\""
}

PROJECT_HOME="$(cd "$(dirname "${BASH_SOURCE[0]}")" && cd ../ && pwd)"
INFER_DIR=$PROJECT_HOME/tracer-infer

OUT_DIR=$PROJECT_HOME/result
TIMEOUT=3600

while getopts ":p:o:t:i:" opt; do
  case $opt in
  p) PACKAGE=$OPTARG ;;
  o) OUT_DIR=$(readlink -e $OPTARG) ;;
  t) TIMEOUT=$OPTARG ;;
  i) INFER_OPTIONS=$OPTARG ;;
  \?)
    echo >&2 "Invalid option -$OPTARG"
    usage
    exit 1
    ;;
  :)
    echo >&2 "Option -$OPTARG requires an argument."
    usage
    exit 1
    ;;
  esac
done

if [ -z "$PACKAGE" ]; then
  echo >&2 "Package name must be given!"
  usage
  exit 1
fi

if [ ! -d "$OUT_DIR" ]; then
  echo >&2 "Output directory is not exist!"
  usage
  exit 1
fi

INFER_BIN=../tracer-infer/infer/bin/infer

pushd $PACKAGE
$INFER_BIN capture -- make
make clean
popd

RESULT_DIR=$PROJECT_HOME/infer-result/$PACKAGE
mkdir $RESULT_DIR
cp -r $PACKAGE/infer-out $RESULT_DIR

pushd $RESULT_DIR
timeout $TIMEOUT $INFER_DIR/infer/bin/infer analyze $INFER_OPTIONS
popd
