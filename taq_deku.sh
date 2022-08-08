#! /usr/bin/env bash

set -e

build_taq(){
    cd taq_deku && npm install && npm run build
}


case "$1" in

build_taq)
    build_taq
;;

esac