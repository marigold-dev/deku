#! /usr/bin/env bash

set -e

# shellcheck source=./tilt/variables.sh
source "./tilt/variables.sh"

disable_nodes() {
  for i in "${NODES[@]}"; do
    tilt disable "deku-node-$i"
    wait $!
  done
}

enable_nodes() {
  for i in "${NODES[@]}"; do
    tilt enable "deku-node-$i"
    tilt wait --for=condition=Ready "uiresource/deku-node-$i"
    sleep 1
  done
}

disable_nodes
tilt trigger deku-tear-down
sleep 1
tilt wait --for=condition=Ready "uiresource/deku-tear-down"

tilt trigger deku-setup
sleep 1
tilt wait --for=condition=Ready "uiresource/deku-setup"

sleep 1
enable_nodes

sleep 5
wait $!
tilt trigger deku-net
