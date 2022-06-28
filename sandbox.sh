#! /usr/bin/env bash

set -e

# =======================
# Parameters declarations

# Starting from 0
NUMBER_OF_NODES=${3:-"3"}

# This secret key never changes.
# We need this secret key for sigining Tezos operations.
# We have to give the secret key to the Deku node as part of configuration.
SECRET_KEY="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"

# Folder containing the data of each node:
# - identity.json
# - pre_epoch_state.json
# - state.bin
# - tezos.json
# - trusted-validator-membership-change.json
# - validators.json
DATA_DIRECTORY="data"

# https://github.com/koalaman/shellcheck/wiki/SC2207
# shellcheck disable=SC2207
VALIDATORS=( $(seq 0 "$((NUMBER_OF_NODES - 1))") )

# =======================
# Running environments
# Docker mode is to run Deku nodes from docker-compose file
if [ "${2:-local}" = "docker" ]; then
  mode="docker"
else
  mode="local"
fi

# =======================
# We need the tezos-client to deploy contracts.
# We will use the binary included in the Tezos testnet deployment
# See https://tezos.gitlab.io/introduction/howtouse.html#client
tezos-client() {
  docker exec -t deku_flextesa tezos-client "$@"
}

consensus_wallet="myWallet"

# Used to interact with ticket contract to prevent clashes in tezos-client counter
ticket_wallet="bob"

if [ "${REBUILD:-}" ]; then
  dune build @install
fi

if [ $mode = "docker" ]; then
  # Flextesa: Flexible Tezos Sandboxes
  # https://gitlab.com/tezos/flextesa
  RPC_NODE=http://flextesa:20000
else
  # Using a Tezos node on localhost:20000 that is provided by the docker-compose file
  RPC_NODE=http://localhost:20000
fi

# =======================
# Helper to print message

message() {
  echo -e "\e[35m\e[1m**************************    $*    ********************************\e[0m"
}

# =======================
# Generate Json files

# validators_json:
# We configure the nodes to be aware of each other.
# The validator addresses in Deku are not Tezos addresses.
# Their addresses begin with `tz1` or `tz2`.
# These addresses are to support Tezos' hashing scheme.
# They don't correlate to actual Tezos wallets.
discovery_storage() {
  ## most of the noise in this function is because of indentation
  echo "Big_map.literal ["
  for VALIDATOR in "${VALIDATORS[@]}"; do
    ADDRESS=$(echo "$VALIDATOR" | awk -F';' '{ print $4 }')
    URI=$(echo "$VALIDATOR" | awk -F';' '{ print $3 }')
    cat <<EOF
  (("$ADDRESS" : key_hash), (0, "$URI"));
EOF
  done
  echo "]"
}

# trusted_validator_membership_change_json:
# Node operators must manually allow for each change to the list
# of validators, otherwise the node will not sign a requested change.
# This information will be stored in the trusted_validator_membership_change.json
# This list is provided at the moment merely as a convenience for
# more efficient hacking on Deku.
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
  tezos-client --endpoint $RPC_NODE originate contract "$1" \
    transferring 0 from $consensus_wallet \
    running "$contract" \
    --init "$storage" \
    --burn-cap 2 \
    --force
}

# Creates deku validators' data directories in $DATA_DIRECTORY,
# compiles and deploys the consensus and validators contracts,
# creates the trusted_validator_membership_change JSON file in $DATA_DIRECTORY subfolders,
# writes Tezos identity files in $DATA_DIRECTORY subfolders
create_new_deku_environment() {

  # Step 1: Set up validator identities for each node
  message "Creating validator identities"
  for i in "${VALIDATORS[@]}"; do
    FOLDER="$DATA_DIRECTORY/$i"
    mkdir -p "$FOLDER"

    if [ $mode = "docker" ]; then
      # Using the deku-cli to generate identities for each node
      # It is important to decide the URI of each node with the `--uri` flag
      # For the current setup, we are using either localhost or deku-node-i and
      # run on different ports (incremental).
      # In future, one can configure URI to be whatever one wish
      deku-cli setup-identity "$FOLDER" --uri "http://deku-node-$i:4440"
    else
      deku-cli setup-identity "$FOLDER" --uri "http://localhost:444$i"
    fi
    KEY=$(deku-cli self "$FOLDER" | grep "key:" | awk '{ print $2 }')
    ADDRESS=$(deku-cli self "$FOLDER" | grep "address:" | awk '{ print $2 }')
    URI=$(deku-cli self "$FOLDER" | grep "uri:" | awk '{ print $2 }')
    VALIDATORS[$i]="$i;$KEY;$URI;$ADDRESS"
  done

  consensus_storage=$(
  # Step 3: After having the Deku identities, we will configure and deploy
  # a Deku consensus contract to the Tezos testnet.
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
    known_handles_hash = (Big_map.empty : Parameter.Types.vault_known_handles_hash_set);
    used_handles = (Big_map.empty : Parameter.Types.vault_used_handle_set);
    vault = (Big_map.empty : Parameter.Types.vault);
  }
}
EOF

)

  # Step 4: deploying the consensus and validators discovery contracts
  consensus="./layer1/bridge/consensus/main.mligo"
  discovery="./src/tezos_interop/discovery.mligo"
  deploy_contract "consensus" "$consensus" "$consensus_storage"
  deploy_contract "discovery" "$discovery" "$(discovery_storage)"

  # call validators_json() and trusted_validator_membership_change_json()
  for VALIDATOR in "${VALIDATORS[@]}"; do
    i=$(echo "$VALIDATOR" | awk -F';' '{ print $1 }')
    FOLDER="$DATA_DIRECTORY/$i"
    trusted_validator_membership_change_json >"$FOLDER/trusted-validator-membership-change.json"
  done

  # Step 5: Look up the address of the contract we just deployed.
  message "Getting contract address"
  TEZOS_CONSENSUS_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract consensus | grep KT1 | tr -d '\r')"
  TEZOS_DISCOVERY_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract discovery | grep KT1 | tr -d '\r')"

  # Step 6: Finally we need to configure each Deku node to communicate with the Tezos testnet.
  # This configuration is stored in a file named `tezos.json`, and is created with the `deku-cli setup-tezos``
  message "Configuring Deku nodes"
  for VALIDATOR in "${VALIDATORS[@]}"; do
    i=$(echo "$VALIDATOR" | awk -F';' '{ print $1 }')
    FOLDER="$DATA_DIRECTORY/$i"

    deku-cli setup-tezos "$FOLDER" \
      --tezos_consensus_contract="$TEZOS_CONSENSUS_ADDRESS" \
      --tezos_discovery_contract="$TEZOS_DISCOVERY_ADDRESS" \
      --tezos_rpc_node=$RPC_NODE \
      --tezos_secret="$SECRET_KEY" \
      --unsafe_tezos_required_confirmations 1
  done
  echo -e "\e[32m\e[1m### Tezos Contract address: $TEZOS_CONSENSUS_ADDRESS ###\e[0m"
}

# =======================
# Steps for the command: ./sandbox start
# - start_deku_cluster()
# - wait_for_servers()
SERVERS=()
start_deku_cluster() {
  # The steps are:
  # - Step 1: Lauch a Tezos testnet
  # - Step 2: Configure the Deku nodes that will run in the cluster such that they
  #           are aware of each other
  # (Step 1 and 2 are ./sandbox.sh setup)
  # - Step 3: Start each node
  echo "Starting nodes."
  for i in "${VALIDATORS[@]}"; do
    if [ "$mode" = "local" ]; then
      deku-node "$DATA_DIRECTORY/$i" --verbosity="${DEKU_LOG_VERBOSITY:-debug}" --listen-prometheus="900$i" &
      SERVERS+=($!)
    fi
  done

  sleep 1

  # Step 4: Manually produce the block
  # Produce a block using `deku-cli produce-block`
  # See deku-cli produce-block --help
  echo "Producing a block"
  if [ "$mode" = "docker" ]; then
    HASH=$(docker exec -t deku-node-0 /bin/deku-cli produce-block /app/data | sed -n 's/block.hash: \([a-f0-9]*\)/\1/p' | tr -d " \t\n\r")
  else
    HASH=$(deku-cli produce-block "$DATA_DIRECTORY/0" | sed -n 's/block.hash: \([a-f0-9]*\)/\1/p')
  fi

  sleep 0.1

  # Step 5: Manually sign the block with 2/3rd of the nodes
  # Sign the previously produced block using `deku-cli sign-block`
  # See ./src/bin/deku_cli.ml:sign_block
  echo "Signing"
  for i in "${VALIDATORS[@]}"; do
    if [ "$mode" = "docker" ]; then
      echo "hash: $HASH"
      echo "deku-node-$i"
      docker exec -t "deku-node-$i" deku-cli sign-block /app/data "$HASH"
    else
      echo "hash: $HASH"
      echo "deku-node-$i"
      deku-cli sign-block "$DATA_DIRECTORY/$i" "$HASH"
    fi
  done

}

wait_for_servers() {
  for PID in "${SERVERS[@]}"; do
    wait "$PID"
  done
}

# =======================
# Steps for the command: ./sandbox.sh smoke-test
# - deku_height()
# - start_deku_cluster()
# - pkill -x deku-node
# - assert_deku_state()
deku_storage() {
  local contract
  contract=$(jq <"$DATA_DIRECTORY/0/tezos.json" '.consensus_contract' | xargs)
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
    asserter "$DATA_DIRECTORY/$i" "$current_state_hash" "$minimum_expected_height"
  done
}

# =======================
# ./sandbox.sh tear-down
# Removes the DATA_DIRECTORY subfolders
# This avoids having wrong state when starting again
# tear-down will be called first when start_tezos_node()
tear-down() {
  for i in "${VALIDATORS[@]}"; do
    FOLDER="$DATA_DIRECTORY/$i"
    if [ -d "$FOLDER" ]; then
      rm -r "$FOLDER"
    fi
  done
}

# =======================
# Steps for the command: ./sandbox.sh setup
# - start_tezos_node ()
# - create_new_deku_environment()

# We need a Tezos wallet to deploy the contract with.
# For convenience, we are using a hard-code secret key to create this wallet

start_tezos_node() {
  tear-down
  message "Configuring Tezos client"
  tezos-client --endpoint $RPC_NODE bootstrapped
  tezos-client --endpoint $RPC_NODE config update
  tezos-client --endpoint $RPC_NODE import secret key myWallet "unencrypted:$SECRET_KEY" --force
}
deploy_dummy_ticket() {
  contract=$(ligo compile contract ./dummy_ticket.mligo)
  tezos-client --endpoint $RPC_NODE originate contract "dummy_ticket" \
    transferring 0 from $ticket_wallet \
    running "$contract" \
    --init "Unit" \
    --burn-cap 2 \
    --force
}



# =======================
# A hard-coded Deku wallet to use in development
DEKU_ADDRESS="tz1RPNjHPWuM8ryS5LDttkHdM321t85dSqaf"
DEKU_PRIVATE_KEY="edsk36FhrZwFVKpkdmouNmcwkAJ9XgSnE5TFHA7MqnmZ93iczDhQLK"
deposit_ticket() {
  CONSENSUS_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract consensus | grep KT1 | tr -d '\r')"
  tezos-client --endpoint $RPC_NODE transfer 0 from $ticket_wallet to dummy_ticket \
  --entrypoint mint_to_deku --arg "Pair (Pair \"$CONSENSUS_ADDRESS\" \"$DEKU_ADDRESS\") (Pair 100 0x)" \
  --burn-cap 2
}

deposit_withdraw_test() {
  # Deposit 100 tickets
  deposit_ticket | grep tezos-client | tr -d '\r'
  sleep 10

  echo "{\"address\": \"$DEKU_ADDRESS\", \"priv_key\": \"$DEKU_PRIVATE_KEY\"}" > wallet.json

  DUMMY_TICKET=$(tezos-client show known contract dummy_ticket | tr -d '\t\n\r')

  # Check that the ticket has been deposited on deku
  BALANCE=$(deku-cli get-balance data/0 $DEKU_ADDRESS "(Pair \"$DUMMY_TICKET\" 0x)" | sed -n "s/Balance: \([0-9]*\)/\1/p")
  if ((BALANCE == 0))
  then
    echo "error: Balance for ticket $DUMMY_TICKET is \"$BALANCE\"! Did the deposit fail?"
    pkill -x deku-node
    exit 1
  fi

  # # We can withdraw 10 tickets from deku
  OPERATION_HASH=$(deku-cli withdraw data/0 ./wallet.json "$DUMMY_TICKET" 10 "Pair \"$DUMMY_TICKET\" 0x" | awk '{ print $2 }' | tr -d '\t\n\r')
  sleep 10

  WITHDRAW_PROOF=$(deku-cli withdraw-proof data/0 "$OPERATION_HASH" "$DUMMY_TICKET%burn_callback" | tr -d '\t\n\r')
  if [ -z "$WITHDRAW_PROOF" ]; then
    echo Withdraw failed!
    pkill -x deku-node
    exit 1
  fi
  sleep 10

  PROOF=$(echo "$WITHDRAW_PROOF" | sed -n 's/.*\({.*}\).*/\1/p')
  ID=$(echo "$WITHDRAW_PROOF" | sed -n 's/.*[[:space:]]\([0-9]\+\)[[:space:]]\".*/\1/p')
  HANDLE_HASH=$(echo "$WITHDRAW_PROOF" | sed -n 's/.*\(0x.*\).*{.*/\1/p')

  CONSENSUS_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract consensus | grep KT1 | tr -d '\r')"

  tezos-client transfer 0 from $ticket_wallet to dummy_ticket --entrypoint withdraw_from_deku --arg "Pair (Pair \"$CONSENSUS_ADDRESS\" (Pair (Pair (Pair 10 0x) (Pair $ID \"$DUMMY_TICKET\")) \"$DUMMY_TICKET\")) (Pair $HANDLE_HASH $PROOF)" --burn-cap 2
}

help() {
  # FIXME: fix these docs
  echo "$0 automates deployment of a Tezos testnet node and setup of a Deku cluster."
  echo ""
  echo "Usage: $0 setup|start|tear-down|smoke-test"
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
}

message "Running in $mode mode"

case "$1" in
setup)
  start_tezos_node
  create_new_deku_environment
  echo -e "\e[33m\e[1m### Warning! ###\e[0m"
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
  pkill -x deku-node
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
  pkill -x deku-node
  ;;
deploy-dummy-ticket)
  deploy_dummy_ticket
  ;;
deposit-dummy-ticket)
  deposit_ticket
  ;;
*)
  help
  ;;
esac
