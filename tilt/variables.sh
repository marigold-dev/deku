#! /usr/bin/env bash

# Starting from 0
NUMBER_OF_NODES=${3:-"3"}

# This secret key never changes.
# We need this secret key for sigining Tezos operations.
# We have to give the secret key to the Deku node as part of configuration.
SECRET_KEY="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"

consensus_wallet="myWallet"

# Used to interact with ticket contract to prevent clashes in tezos-client counter
ticket_wallet="bob"

if [ "${2:-local}" = "docker" ]; then
  mode="docker"
else
  mode="local"
fi

if [ $mode = "docker" ]; then
  # Flextesa: Flexible Tezos Sandboxes
  # https://gitlab.com/tezos/flextesa
  RPC_NODE=http://flextesa:20000
else
  # Using a Tezos node on localhost:20000 that is provided by the docker-compose file
  RPC_NODE=http://localhost:20000
fi

DATA_DIRECTORY="data"

# https://github.com/koalaman/shellcheck/wiki/SC2207
# shellcheck disable=SC2207
NODES=( $(seq 0 "$((NUMBER_OF_NODES - 1))") )
