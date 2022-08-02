#!/usr/bin/env bash

RPC_NODE=${RPC_NODE:-http://localhost:20000} 
DEKU_TEZOS_CONSENSUS_ADDRESS="$(tezos-client --endpoint "$RPC_NODE" show known contract consensus | grep KT1 | tr -d '\r')"
export DEKU_TEZOS_CONSENSUS_ADDRESS
DEKU_TEZOS_DISCOVERY_ADDRESS="$(tezos-client --endpoint "$RPC_NODE" show known contract discovery | grep KT1 | tr -d '\r')"
export DEKU_TEZOS_DISCOVERY_ADDRESS

DEKU_VALIDATOR_URIS="http://localhost:4440,http://localhost:4441,http://localhost:4442,http://localhost:4443"
export DEKU_VALIDATOR_URIS

for NODE in {0..3}
do
    source "./configs/flextesa/node_${NODE}_env"
    mkdir -p "./data/$NODE"
    deku-node ./configs/flextesa/config.json &
done

# Run the bootstrapper
source ./configs/flextesa/bootstrapper_secret_env
deku-bootstrapper
