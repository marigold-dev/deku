#!/usr/bin/env bash

trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

vm=${1:-"wasm-vm"}

dune build || exit 1

dir=$(dirname $0)
source $dir/scripts/common.sh

export DEKU_TEZOS_RPC_NODE=${DEKU_TEZOS_RPC_NODE:-http://localhost:20000}
message "Using Tezos RPC Node: $DEKU_TEZOS_RPC_NODE"

export DEKU_VALIDATORS="tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK,tz1PYdVbnLwiqKo3fLFXTKxw6K7BhpddQPh8,tz1Pv4viWq7ye4R6cr9SKR3tXiZGvpK34SKi,tz1cXKCCxLwYCHDSrx9hfD5Qmbs4W8w2UKDw"
export DEKU_VALIDATOR_URIS="127.0.0.1:4440,127.0.0.1:4441,127.0.0.1:4442,127.0.0.1:4443"
export DEKU_TEZOS_SECRET="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"
export DEKU_TEZOS_CONSENSUS_ADDRESS="KT1LHcxdRTgyFp1TdrgodVekLFkQwzFnTJcY"
export DEKU_DUMMYT_TICKET="KT1Us9LZaG8F6cskmMg1hB2FPRwakWkegkPi"
export DEKU_API_PORT=8080
export DEKU_DEFAULT_BLOCK_SIZE=${DEKU_DEFAULT_BLOCK_SIZE:-10000}
export DEKU_LOG_VERBOSITY=${DEKU_LOG_VERBOSITY:-info}

# Starting only one API node
export DEKU_API_NODE_URI="127.0.0.1:4440"
export DEKU_API_PORT=8080
export DEKU_API_DATABASE_URI="sqlite3:/tmp/api_database.db"
export DEKU_API_DOMAINS=8
export DEKU_API_VM="./flextesa_chain/data/0/api_vm_pipe"
export DEKU_API_DATA_FOLDER="./flextesa_chain/data/0/"

## The api needs its own vm
# nix run ".#$vm" -- "$DEKU_API_VM" &
_build/install/default/bin/deku-api &

sleep 3

for N in 0 1 2 3; do
  source "./networks/flextesa/node_${N}_env"

  mkdir -p ./flextesa_chain/data/$N

  # Creates the FIFO
  # test -p "./flextesa_chain/data/$N/pipe_write" || mkfifo "./flextesa_chain/data/$N/pipe_write"
  # test -p "./flextesa_chain/data/$N/pipe_read" || mkfifo "./flextesa_chain/data/$N/pipe_read"

  # Starts the VM
  # nix run ".#$vm" -- "./flextesa_chain/data/$N/pipe" &

  sleep 0.4

  # Starts the Node
  _build/install/default/bin/deku-node \
    --default-block-size=10000 \
    --port "444$N" \
    --database-uri "sqlite3:./flextesa_chain/data/$N/database.db" \
    --named-pipe-path "./flextesa_chain/data/$N/pipe" \
    --data-folder "./flextesa_chain/data/$N" &
  sleep 0.1
done

wait
