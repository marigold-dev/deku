#! /usr/bin/env bash

# shellcheck source=./tilt/variables.sh
source "./tilt/variables.sh"

echo "$(pwd)"

echo "$RPC_NODE"



# =======================
# We need the tezos-client to deploy contracts.
# We will use the binary included in the Tezos testnet deployment
# See https://tezos.gitlab.io/introduction/howtouse.html#client
tezos-client() {
  docker exec -t deku_flextesa tezos-client "$@"
}
  
tezos-client --endpoint "$RPC_NODE" bootstrapped
tezos-client --endpoint "$RPC_NODE" config update
tezos-client --endpoint "$RPC_NODE" import secret key myWallet "unencrypted:$SECRET_KEY" --force