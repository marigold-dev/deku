#! /usr/bin/env bash

dir=$(dirname $0)
source $dir/common.sh

# This secret key never changes.
# We need this secret key for sigining Tezos operations.
SECRET_KEY="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"

# We need the tezos-client to deploy contracts.
# We will use the binary included in the Tezos testnet deployment
# See https://tezos.gitlab.io/introduction/howtouse.html#client
tezos-client() {
  docker exec -t deku_flextesa tezos-client "$@"
}

# Using a Tezos node on localhost:20000 that is provided by the docker-compose file
DEKU_TEZOS_RPC_NODE=${DEKU_TEZOS_RPC_NODE:-http://localhost:20000}

message "Configuring Tezos client"
tezos-client --endpoint "$DEKU_TEZOS_RPC_NODE" bootstrapped
tezos-client --endpoint "$DEKU_TEZOS_RPC_NODE" config update
tezos-client --endpoint "$DEKU_TEZOS_RPC_NODE" import secret key myWallet "unencrypted:$SECRET_KEY" --force

# [deploy_contract name source_file initial_storage] compiles the Ligo code in [source_file],
# the [initial_storage] expression and originates the contract as myWallet on Tezos.
deploy_contract () {
  message "Deploying new $1 contract"

  # Compiles an initial storage for a given contract to a Michelson expression.
  # The resulting Michelson expression can be passed as an argument in a transaction which originates a contract.
  storage=$(ligo compile storage "$2" "$3")

  # Compiles a contract to Michelson code.
  # Expects a source file and an entrypoint function.
  contract=$(ligo compile contract "$2")

  echo "Originating $1 contract"
  sleep 2
  tezos-client --endpoint "$DEKU_TEZOS_RPC_NODE" originate contract "$1" \
    transferring 0 from myWallet \
    running "$contract" \
    --init "$storage" \
    --burn-cap 2 \
    --force
}


deploy_contract "consensus" \
    "./src/tezos_interop/consensus.mligo" \
    "$(cat ./configs/flextesa/consensus_storage.mligo)"

deploy_contract "discovery" \
    "./src/tezos_interop/discovery.mligo" \
    "$(cat ./configs/flextesa/discovery_storage.mligo)"

deploy_contract "dummy_ticket" \
    "./dummy_ticket.mligo" \
    "()"

wait
