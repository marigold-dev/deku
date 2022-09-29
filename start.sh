#!/usr/bin/env bash

trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

vm=${1:-"wasm-vm"}

dune build || exit 1

dir=$(dirname $0)
source $dir/scripts/common.sh

export DEKU_TEZOS_RPC_NODE=${DEKU_TEZOS_RPC_NODE:-http://localhost:20000}
message "Using Tezos RPC Node: $DEKU_TEZOS_RPC_NODE"

export DEKU_VALIDATORS="tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK,tz1PYdVbnLwiqKo3fLFXTKxw6K7BhpddQPh8,tz1Pv4viWq7ye4R6cr9SKR3tXiZGvpK34SKi,tz1cXKCCxLwYCHDSrx9hfD5Qmbs4W8w2UKDw"
export DEKU_VALIDATOR_URIS="localhost:4440,localhost:4441,localhost:4442,localhost:4443"
export DEKU_TEZOS_SECRET="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"
export DEKU_TEZOS_CONSENSUS_ADDRESS="$(tezos_client --endpoint $DEKU_TEZOS_RPC_NODE show known contract consensus | grep KT1 | tr -d '\r')"
export DEKU_API_PORT=8080

for N in 0 1 2 3; do
  source "./chain/data/$N/env"

  # Creates the FIFO
  test -p "./chain/data/$N/pipe_write" || mkfifo "./chain/data/$N/pipe_write"
  test -p "./chain/data/$N/pipe_read" || mkfifo "./chain/data/$N/pipe_read"

  # Starts the VM
  nix run ".#$vm" -- "./chain/data/$N/pipe" &

  sleep 0.4

  # Starts the Node
  _build/install/default/bin/deku-node \
    --default-block-size=100 \
    --port "444$N" \
    --database-uri "sqlite3:./chain/data/$N/database.db" \
    --named-pipe-path "./chain/data/$N/pipe" \
    --data-folder "./chain/data/$N" &
  sleep 0.1
done

# Only starting one API
# export DEKU_API_DEKU_NODE="http://localhost:4440"
# _build/install/default/bin/deku-api --database-uri "sqlite3:/tmp/database.db" &

wait
