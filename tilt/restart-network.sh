#! /usr/bin/env bash

set -e

NUMBER_OF_NODES=${1:-"3"}

# https://github.com/koalaman/shellcheck/wiki/SC2207
# shellcheck disable=SC2207
NODES=( $(seq 0 "$((NUMBER_OF_NODES - 1))") )

for i in "${NODES[@]}"; do
  tilt disable "deku-node-$i"
done

tilt trigger deku-tear-down
tilt wait --for=condition=Ready "uiresource/deku-tear-down"
tilt trigger deku-setup

sleep 2

# tilt wait --for=condition=Ready "uiresource/deku-setup"

for i in "${NODES[@]}"; do
  tilt enable "deku-node-$i"
done

tilt trigger deku-net
