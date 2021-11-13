#!/bin/sh
set -e
set -x

opam exec -- dune build -p ligo

# TODO: also try instead from time to time:
#- (cd ./src/; dune build -p ligo)
