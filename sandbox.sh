#! /usr/bin/env bash

set -e

if [ "${2:-local}" = "docker" ]; then
  mode="docker"
else
  mode="local"
fi

NUMBER_OF_NODES=${3:-"3"}

data_directory="data"

# shellcheck disable=SC2016
[ "$USE_NIX" ] || LD_LIBRARY_PATH=$(esy x sh -c 'echo $LD_LIBRARY_PATH')
export LD_LIBRARY_PATH
# shellcheck disable=SC2016
[ "$USE_NIX" ] || PATH=$(esy x sh -c 'echo $PATH')
export PATH

if [ "${REBUILD:-}" ]; then
  if [ "$USE_NIX" ]; then
    dune build @install
  else
    esy dune build @install
  fi
fi

tezos-client() {
  docker exec -t deku_flextesa tezos-client "$@"
}

if ! [ "$USE_NIX" ]; then
  ligo() {
    docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:0.37.0 "$@"
  }
fi

if [ $mode = "docker" ]; then
  RPC_NODE=http://flextesa:20000
else
  RPC_NODE=http://localhost:20000
fi

# This secret key never changes.
SECRET_KEY="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"

DATA_DIRECTORY="data"

# shellcheck disable=SC2207
VALIDATORS=($(seq 0 "$NUMBER_OF_NODES"))

message() {
  echo -e "\e[35m\e[1m**************************    $*    ********************************\e[0m"
}

validators_json() {
  ## most of the noise in this function is because of indentation
  echo "["
  for VALIDATOR in "${VALIDATORS[@]}"; do
    i=$(echo "$VALIDATOR" | awk -F';' '{ print $1 }')
    ADDRESS=$(echo "$VALIDATOR" | awk -F';' '{ print $4 }')
    URI=$(echo "$VALIDATOR" | awk -F';' '{ print $3 }')
    if [ "$i" != 0 ]; then
      printf ",
"
    fi
    cat <<EOF
  {
    "address": "$ADDRESS",
    "uri": "$URI"
EOF
    printf "  }"
  done
  echo ""
  echo "]"
}

trusted_validator_membership_change_json() {
  echo "["
  for VALIDATOR in "${VALIDATORS[@]}"; do
    i=$(echo "$VALIDATOR" | awk -F';' '{ print $1 }')
    ADDRESS=$(echo "$VALIDATOR" | awk -F';' '{ print $4 }')
    if [ "$i" != 0 ]; then
      printf ",
"
    fi
    cat <<EOF
  {
    "action": [ "Add" ],
    "address": "$ADDRESS"
EOF
    printf "  }"
  done
  echo ""
  echo "]"
}

create_new_deku_environment() {
  message "Creating validator identities"
  for i in "${VALIDATORS[@]}"; do
    FOLDER="$DATA_DIRECTORY/$i"
    mkdir -p "$FOLDER"

    if [ $mode = "docker" ]; then
      deku-cli setup-identity "$FOLDER" --uri "http://deku-node-$i:4440"
    else
      deku-cli setup-identity "$FOLDER" --uri "http://localhost:444$i"
    fi
    KEY=$(deku-cli self "$FOLDER" | grep "key:" | awk '{ print $2 }')
    ADDRESS=$(deku-cli self "$FOLDER" | grep "address:" | awk '{ print $2 }')
    URI=$(deku-cli self "$FOLDER" | grep "uri:" | awk '{ print $2 }')
    VALIDATORS[$i]="$i;$KEY;$URI;$ADDRESS"
  done

  message "Deploying new consensus contract"

  # To register the validators, run consensus.mligo with the list of
  # validators. To do this quickly, open the LIGO IDE with the url
  # provided and paste the following storage as inputs to the contract.
  storage=$(
    cat <<EOF
{
  root_hash = {
    current_block_hash = 0x;
    current_block_height = 0;
    current_state_hash = 0x;
    current_handles_hash = 0x;
    current_validators = [
EOF
    ## this iteration is done here just to ensure the indentation
    for VALIDATOR in "${VALIDATORS[@]}"; do
      ADDRESS=$(echo "$VALIDATOR" | awk -F';' '{ print $4 }')
      echo "      (\"$ADDRESS\": key_hash);"
    done
    cat <<EOF
    ];
  };
  vault = {
    known_handles_hash = (Big_map.empty : vault_known_handles_hash_set);
    used_handles = (Big_map.empty : vault_used_handle_set);
    vault = (Big_map.empty : vault);
  }
}
EOF
  )

  consensus="./src/tezos_interop/consensus.mligo"
  storage=$(ligo compile storage "$consensus" "$storage")
  contract=$(ligo compile contract $consensus)

  message "Originating contract"
  sleep 2
  tezos-client --endpoint $RPC_NODE originate contract "consensus" \
    transferring 0 from myWallet \
    running "$contract" \
    --init "$storage" \
    --burn-cap 2 \
    --force

  for VALIDATOR in "${VALIDATORS[@]}"; do
    i=$(echo "$VALIDATOR" | awk -F';' '{ print $1 }')
    FOLDER="$DATA_DIRECTORY/$i"
    validators_json >"$FOLDER/validators.json"
    trusted_validator_membership_change_json >"$FOLDER/trusted-validator-membership-change.json"
  done

  message "Getting contract address"
  CONSENSUS_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract consensus | grep KT1 | tr -d '\r')"

  message "Configuring Deku nodes"
  for VALIDATOR in "${VALIDATORS[@]}"; do
    i=$(echo "$VALIDATOR" | awk -F';' '{ print $1 }')
    FOLDER="$DATA_DIRECTORY/$i"

    deku-cli setup-tezos "$FOLDER" \
      --tezos_consensus_contract="$CONSENSUS_ADDRESS" \
      --tezos_rpc_node=$RPC_NODE \
      --tezos_secret="$SECRET_KEY" \
      --unsafe_tezos_required_confirmations 1
  done
  echo -e "\e[32m\e[1m### Tezos Contract address: $TEZOS_CONSENSUS_ADDRESS ###\e[0m"
}

tear-down() {
  for i in "${VALIDATORS[@]}"; do
    FOLDER="$DATA_DIRECTORY/$i"
    if [ -d "$FOLDER" ]; then
      rm -r "$FOLDER"
    fi
  done
}

start_tezos_node() {
  tear-down
  message "Configuring Tezos client"
  tezos-client --endpoint $RPC_NODE bootstrapped
  tezos-client --endpoint $RPC_NODE config update
  tezos-client --endpoint $RPC_NODE import secret key myWallet "unencrypted:$SECRET_KEY" --force
}

SERVERS=()
start_deku_cluster() {
  echo "Starting nodes."
  for i in "${VALIDATORS[@]}"; do
    if [ "$mode" = "local" ]; then
      deku-node "$data_directory/$i" "$data_directory/state_transition" --listen-prometheus="900$i" &
      SERVERS+=($!)
    fi
  done

  sleep 1

  echo "Producing a block"
  if [ "$mode" = "docker" ]; then
    HASH=$(docker exec -t deku-node-0 deku-cli produce-block /app/data | awk '{ print $2 }' | tail -n1 | tr -d " \t\n\r")
  else
    HASH=$(deku-cli produce-block "$data_directory/0" | awk '{ print $2 }')
  fi

  sleep 0.1

  echo "Signing"
  for i in "${VALIDATORS[@]}"; do
    if [ "$mode" = "docker" ]; then
      echo "hash: $HASH"
      echo "deku-node-$i"
      docker exec -t "deku-node-$i" deku-cli sign-block /app/data "$HASH"
    else
      deku-cli sign-block "$data_directory/$i" "$HASH"
    fi
  done

}

wait_for_servers() {
  for PID in "${SERVERS[@]}"; do
    wait "$PID"
  done
}

deku_storage() {
  local contract
  contract=$(jq <"$data_directory/0/tezos.json" '.consensus_contract' | xargs)
  local storage
  storage=$(curl --silent "$RPC_NODE/chains/main/blocks/head/context/contracts/$contract/storage")
  echo "$storage"
}

deku_state_hash() {
  local storage
  storage=$(deku_storage)
  local state_hash
  state_hash=$(echo "$storage" | jq '.args[0].args[0].args[2].bytes' | xargs)
  echo "$state_hash"
}

deku_height() {
  local storage
  storage=$(deku_storage)
  local block_height
  block_height=$(echo "$storage" | jq '.args[0].args[0].args[0].args[1].int' | xargs)
  echo "$block_height"
}

assert_deku_state() {
  local current_state_hash
  current_state_hash=$(deku_state_hash)
  local current_block_height
  current_block_height=$(deku_height)
  local starting_height=$1
  local seconds=$2
  local minimum_expected_height=$((starting_height + $2))

  echo "The current block height is" "$current_block_height"

  # Check that a current height has progressed past the starting height sufficiently
  if [ $((current_block_height - starting_height)) -lt 20 ]; then
    echo "Error: less than 20 blocks were produced in $2 seconds."
    exit 1
  fi

  for i in "${VALIDATORS[@]}"; do
    asserter "$data_directory/$i" "$current_state_hash" "$minimum_expected_height"
  done
}

deploy_dummy_ticket() {
  contract=$(ligo compile contract ./dummy_ticket.mligo)
  tezos-client --endpoint $RPC_NODE originate contract "dummy_ticket" \
    transferring 0 from myWallet \
    running "$contract" \
    --init "Unit" \
    --burn-cap 2 \
    --force
}

# A hard-coded Deku wallet to use in development
DEKU_ADDRESS="tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf"
deposit_ticket() {
  CONSENSUS_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract consensus | grep KT1 | tr -d '\r')"
  tezos-client --endpoint $RPC_NODE transfer 0 from myWallet to dummy_ticket \
    --entrypoint deposit --arg "Pair (Pair 0x \"$CONSENSUS_ADDRESS\") \"$DEKU_ADDRESS\"" \
    --burn-cap 2
}

load_test() {
  DUMMY_TICKET_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract dummy_ticket | grep KT1 | tr -d '\r')"
  node_pid=$(ps -A | grep deku-node | head -n 1 | cut -d ' ' -f 1)
  # TODO: make load-test poll to know when to exit.
  perf record -g -p "$node_pid" sh -c "deku-load-test \"saturate\" \"$DUMMY_TICKET_ADDRESS\" && sleep 10"
  perf script -i perf.data >profile.linux-perf.txt
}

deposit_withdraw_test() {
  # Deposit 100 tickets
  deposit_ticket | grep tezos-client | tr -d '\r'
  sleep 10

  echo "{\"address\": \"$DEKU_ADDRESS\", \"priv_key\": \"edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK\"}" > wallet.json

  DUMMY_TICKET=$(tezos-client show known contract dummy_ticket | tr -d '\t\n\r')

  # # We can withdraw 10 tickets from deku
  OPERATION_HASH=$(deku-cli withdraw data/0 ./wallet.json "$DUMMY_TICKET" 10 "Pair \"$DUMMY_TICKET\" 0x" | awk '{ print $2 }' | tr -d '\t\n\r')
  sleep 10

  WITHDRAW_PROOF=$(deku-cli withdraw-proof data/0 "$OPERATION_HASH" "$DUMMY_TICKET%burn_callback" | tr -d '\t\n\r')
  sleep 10

  PROOF=$(echo "$WITHDRAW_PROOF" | sed -n 's/.*\({.*}\).*/\1/p')
  ID=$(echo "$WITHDRAW_PROOF" | sed -n 's/.*[[:space:]]\([0-9]\+\)[[:space:]]\".*/\1/p')
  HANDLE_HASH=$(echo "$WITHDRAW_PROOF" | sed -n 's/.*\(0x.*\).*{.*/\1/p')

  CONSENSUS_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract consensus | grep KT1 | tr -d '\r')"

  tezos-client transfer 0 from myWallet to dummy_ticket --entrypoint withdraw_from_deku --arg "Pair (Pair \"$CONSENSUS_ADDRESS\" (Pair (Pair (Pair 10 0x) (Pair $ID \"$DUMMY_TICKET\")) \"$DUMMY_TICKET\")) (Pair $HANDLE_HASH $PROOF)" --burn-cap 2
}

help() {
  # FIXME: fix these docs
  echo "$0 automates deployment of a Tezos testnet node and setup of a Deku cluster."
  echo ""
  echo "Usage: $0 setup|tear-down"
  echo "Commands:"
  echo "setup"
  echo "  Does the following:"
  echo "  - Starts a Tezos sandbox network with Flextesa."
  echo "  - Generates new validator identities."
  echo "  - Deploys a new contract to the Tezos sandbox configured to use these validators."
  echo "start"
  echo "  Starts a Deku cluster configured with this script."
  echo "tear-down"
  echo "  Stops the Tezos node and destroys the Deku state"
  echo "smoke-test"
  echo "  Starts a Deku cluster and performs some simple checks that its working."
  echo "deploy-dummy-ticket"
  echo "  Deploys a contract that forges dummy tickets and deposits to Deku"
  echo "deposit-withdraw-test"
  echo "  Start a Deku cluster and originate a dummy tickets and performs a deposit and a withdraw"
  echo "deposit-dummy-ticket"
  echo " Executes a deposit of a dummy ticket to Deku"
  echo "load-test (saturate | maximal-blocks)"
  echo "  Performs the specified load test on a running cluster"
}

message "Running in $mode mode"

case "$1" in
setup)
  start_tezos_node
  create_new_deku_environment
  echo -e "\e[33m\e[1m### Warnning! ###\e[0m"
  echo -e "\e[33m\e[1m### This script creates a sandbox node and is for development purposes only. ###\e[0m"
  echo -e "\e[33m\e[1m### It does unsafe things like lowering the required Tezos confirmations to limits unreasonable for production. ###\e[0m"
  echo -e "\e[33m\e[1m### Do not use these settings in production! ###\e[0m"
  ;;
start)
  start_deku_cluster
  wait_for_servers
  ;;
smoke-test)
  starting_height=$(deku_height)
  start_deku_cluster
  seconds=35
  sleep $seconds
  killall deku-node
  assert_deku_state "$starting_height" $seconds
  ;;
tear-down)
  tear-down
  ;;
deposit-withdraw-test)
  start_deku_cluster
  sleep 5
  deploy_dummy_ticket
  deposit_withdraw_test
  killall deku-node
  ;;
deploy-dummy-ticket)
  deploy_dummy_ticket
  ;;
deposit-dummy-ticket)
  deposit_ticket
  ;;
load-test)
  load_test "$2"
  ;;
*)
  help
  ;;
esac