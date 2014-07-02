#!/bin/bash

INITIAL_DIR=$(pwd)
SOURCE="${BASH_SOURCE[0]}"
while [ -h "$SOURCE" ] ; do SOURCE="$(readlink "$SOURCE")"; done
SCRIPT_BASE_DIR="$( cd -P "$( dirname "$SOURCE" )" && pwd )"

INSTALL_DIR=$SCRIPT_BASE_DIR/.emacs.d/site-lisp

cd $INITIAL_DIR
git submodule init && git submodule update

echo "-- Move to source directory ${SCRIPT_BASE_DIR}/install"
cd $SCRIPT_BASE_DIR/install

rm -rf auto-complete/lib/fuzzy auto-complete/lib/popup
ln -s $INSTALL_DIR/fuzzy auto-complete/lib/
ln -s $INSTALL_DIR/popup auto-complete/lib/

for i in android-mode auto-complete
do
    echo "--- Install ${i}"
    cd $i
    [ ! -d $INSTALL_DIR/$i ] && mkdir $INSTALL_DIR/$i
    make install DIR=$INSTALL_DIR/$i
    cd ..
done

cd $INITIAL_DIR
cd .emacs.d/site-lisp/rinari
git submodule init && git submodule update

cd $INITIAL_DIR
cd .emacs.d/site-lisp/yasnippet
git submodule init && git submodule update
gem install plist trollop
rake convert_bundles
rake compile

cd $INITIAL_DIR
cd .emacs.d/site-lisp/autopair
emacs -Q -L . -batch -f batch-byte-compile autopair.el

cd $INITIAL_DIR
cd .emacs.d/site-lisp/emacs-web-server
emacs -Q -L . -batch -f batch-byte-compile simple-httpd.el

cd $INITIAL_DIR
cd .emacs.d/site-lisp/skewer-mode
make compile

cd $INITIAL_DIR
cd .emacs.d/site-lisp/js2-mode
make all

cd $INITIAL_DIR
cd .emacs.d/site-lisp/ac-js2
emacs -Q -L . -L ../js2-mode -L ../skewer-mode -L ../emacs-web-server -batch -f batch-byte-compile ac-js2.el

cd $INITIAL_DIR
cd .emacs.d/site-lisp/nginx-mode
emacs -Q -L . -batch -f batch-byte-compile nginx-mode.el
cd $INITIAL_DIR
