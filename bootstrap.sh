#!/bin/bash
set -e
CMAKE=cmake


WD=$(pwd)
DEP_PATH=$WD/dependency
ZMQ_BP=$DEP_PATH/build_zmq
FZMQ_BP=$DEP_PATH/build_fzmq

git submodule init
git submodule update

mkdir -p "$ZMQ_BP"
mkdir -p "$FZMQ_BP"
cd "$ZMQ_BP"
$CMAKE "$WD/libzmq" -DCMAKE_INSTALL_PREFIX="$DEP_PATH"
make -j4
make install

cd "$FZMQ_BP"
$CMAKE "$WD/fzmq" -DCMAKE_INSTALL_PREFIX="$DEP_PATH"
make -j4
make install


