#! /usr/bin/env bash

# FIXME: throw this script away - we don't want more scripts
# FYI, to run this script, you need to deploy fresh contracts first
# i.e ./scripts/deploy_contracts.sh && ./scripts/test_tezos_interop.sh

RPC_NODE=${RPC_NODE:-http://localhost:20000}

tezos-client() {
  docker exec -t deku_flextesa tezos-client "$@"
}

export DEKU_CONSENSUS_CONTRACT="$(tezos-client --endpoint $RPC_NODE show known contract consensus | grep KT1 | tr -d '\r')"
export DEKU_DISCOVERY_CONTRACT="$(tezos-client --endpoint $RPC_NODE show known contract discovery | grep KT1 | tr -d '\r')"

dune exec ./src/tezos_interop/tests/deku_update_state_root_hash_test.exe   


# TODO: implement deposit test something like this.

# export DEKU_DUMMY_TICKET_CONTRACT="$(tezos-client --endpoint $RPC_NODE show known contract dummy_ticket | grep KT1 | tr -d '\r')"

# dune exec ./src/tezos_interop/tests/deposit_test.exe  &

# export DEKU_ADDRESS="tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf"
# export DEKU_PRIVATE_KEY="edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK"

# ticket_wallet="bob"

# tezos-client --endpoint "$RPC_NODE" transfer 0 from $ticket_wallet to dummy_ticket \
#   --entrypoint mint_to_deku --arg "Pair (Pair \"$DEKU_CONSENSUS_CONTRACT\" \"$DEKU_ADDRESS\") (Pair 100 0x)" \
#   --burn-cap 2

# wait
