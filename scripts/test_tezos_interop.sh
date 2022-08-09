#! /usr/bin/env bash

# FIXME: throw this script away - we don't want more scripts

message() {
  echo -e "\e[35m\e[1m**************************    $*    ********************************\e[0m"
}

RPC_NODE=${RPC_NODE:-http://localhost:20000}

tezos-client() {
  docker exec -t deku_flextesa tezos-client "$@"
}

# FIXME: duplicated form ./deploy_contracts.sh
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
  tezos-client --endpoint "$RPC_NODE" originate contract "$1" \
    transferring 0 from myWallet \
    running "$contract" \
    --init "$storage" \
    --burn-cap 2 \
    --force
}

deploy_contract "test_consensus" \
    "./src/tezos_interop/consensus.mligo" \
    "$(cat ./configs/flextesa/consensus_storage.mligo)"

export DEKU_CONSENSUS_CONTRACT="$(tezos-client --endpoint $RPC_NODE show known contract test_consensus | grep KT1 | tr -d '\r')"

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
