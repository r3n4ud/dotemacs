#!/bin/bash

INITIAL_DIR=$(pwd)
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ] ; do SOURCE="$(readlink "$SOURCE")"; done
SCRIPT_BASE_DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

INSTALL_DIR=$SCRIPT_BASE_DIR/.emacs.d/site-lisp

echo "-- Move to source directory ${SCRIPT_BASE_DIR}/install"
cd $SCRIPT_BASE_DIR/install

for i in android-mode auto-complete
do
    echo "--- Install ${i}"
    cd $i
    [ ! -d $INSTALL_DIR/$i ] && mkdir $INSTALL_DIR/$i
    make install DIR=$INSTALL_DIR/$i
    cd ..
done

cd $INITIAL_DIR

git submodule init && git submodule update

cd .emacs.d/site-lisp/rinari
git submodule init && git submodule update

cd .emacs.d/
emacs -Q -L . -batch -f batch-byte-compile autopair.el

cd $INITIAL_DIR
