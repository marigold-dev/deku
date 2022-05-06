#! /usr/bin/env bash
# shellcheck disable=SC2155

set -e

if [ "$2" = "docker" ]
then
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

[ "$USE_NIX" ] && dune build @install

tezos-client() {
  docker exec -t deku_flextesa tezos-client "$@"
}

ligo() {
  docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:0.28.0 "$@"
}

if [ $mode  = "docker" ]
then
  RPC_NODE=http://flextesa:20000
else
  RPC_NODE=http://localhost:20000
fi

# This secret key never changes.
SECRET_KEY="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"

DATA_DIRECTORY="data"

# shellcheck disable=SC2207
VALIDATORS=( $(seq 0 "$NUMBER_OF_NODES") )

message() {
  echo "=========== $* ==========="
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

    if [ $mode = "docker" ]
    then
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
  TEZOS_CONSENSUS_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract consensus | grep KT1 | tr -d '\r')"

  message "Configuring Deku nodes"
  for VALIDATOR in "${VALIDATORS[@]}"; do
    i=$(echo "$VALIDATOR" | awk -F';' '{ print $1 }')
    FOLDER="$DATA_DIRECTORY/$i"

    deku-cli setup-tezos "$FOLDER" \
      --tezos_consensus_contract="$TEZOS_CONSENSUS_ADDRESS" \
      --tezos_rpc_node=$RPC_NODE \
      --tezos_secret="$SECRET_KEY" \
      --unsafe_tezos_required_confirmations 1
  done
  echo "Tezos Contract address: $TEZOS_CONSENSUS_ADDRESS"
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
    if [ "$mode" = "local" ]
    then
      deku-node "$data_directory/$i" --listen-prometheus="900$i" &
      SERVERS+=($!)
    fi
  done

  sleep 1

  echo "Producing a block"
  if [ "$mode" = "docker" ]
  then
    HASH=$(docker exec -t deku-node-0 deku-cli produce-block /app/data | awk '{ print $2 }' | tail -n1 | tr -d " \t\n\r" )
  else
    HASH=$(deku-cli produce-block "$data_directory/0" | awk '{ print $2 }')
  fi

  sleep 0.1

  echo "Signing"
  for i in "${VALIDATORS[@]}"; do
    if [ "$mode" = "docker" ]
    then
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
  local contract=$(< "$data_directory/0/tezos.json" jq '.consensus_contract' | xargs)
  local storage=$(curl --silent "$RPC_NODE/chains/main/blocks/head/context/contracts/$contract/storage")
  echo "$storage"
}

deku_state_hash() {
  local storage=$(deku_storage)
  local state_hash=$(echo "$storage" | jq '.args[0].args[0].args[2].bytes' | xargs)
  echo "$state_hash"
}

deku_height() {
  local storage=$(deku_storage)
  local block_height=$(echo "$storage" | jq '.args[0].args[0].args[0].args[1].int' | xargs)
  echo "$block_height"
}

assert_deku_state() {
  local current_state_hash=$(deku_state_hash)
  local current_block_height=$(deku_height)
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
}

message "Running in $mode mode"

case "$1" in
setup)
  start_tezos_node
  create_new_deku_environment
  message "Warning"
  echo "This script creates a sandbox node and is for development purposes only."
  echo "It does unsafe things like lowering the required Tezos confirmations to limits unreasonable for production."
  message "Do not use these settings in production!"
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
*)
  help
  ;;
esac
