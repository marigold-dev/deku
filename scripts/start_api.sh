dune build || exit 1

dir=$(dirname $0)
source $dir/common.sh

vm=${1:-"wasm-vm"}

# Starting only one API node for the node 0
export DEKU_TEZOS_RPC_NODE=${DEKU_TEZOS_RPC_NODE:-http://localhost:20000}
export DEKU_TEZOS_CONSENSUS_ADDRESS="KT1LHcxdRTgyFp1TdrgodVekLFkQwzFnTJcY"
export DEKU_API_NODE_URI="127.0.0.1:4440"
export DEKU_API_PORT=8080
export DEKU_API_DATABASE_URI="sqlite3:/tmp/api_database.db"
export DEKU_API_DOMAINS=8
export DEKU_API_VM="./flextesa_chain/data/0/api_vm_pipe"
export DEKU_API_DATA_FOLDER="./flextesa_chain/data/0/"
export DEKU_API_LOG_VERBOSITY=${DEKU_API_LOG_VERBOSITY:-info}

## The api needs its own vm
# nix run ".#$vm" -- "$DEKU_API_VM" &
sleep 1
echo Start the API

# Start the API
./_build/install/default/bin/deku-api
