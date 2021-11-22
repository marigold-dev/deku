#!/bin/sh
set -e
set -x

opam update
# NEW-PROTOCOL-TEMPORARY

# Install local dependencies
opam install -y --deps-only --with-test --locked ./ligo.opam
