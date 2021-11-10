#! /bin/bash

set -e

RPC_NODE=http://localhost:20000

# This secret key never changes.
SECRET_KEY="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"

DATA_DIRECTORY="data"

SIDECLI=$(esy x which sidecli)
sidecli() {
  eval $SIDECLI '"$@"'
}

VALIDATORS=(0 1 2)

message() {
  echo "=========== $@ ==========="
}

validators_json() {
  ## most of the noise in this function is because of indentation
  echo "["
  for VALIDATOR in "${VALIDATORS[@]}"; do
    i=$(echo $VALIDATOR | awk -F';' '{ print $1 }')
    ADDRESS=$(echo $VALIDATOR | awk -F';' '{ print $4 }')
    URI=$(echo $VALIDATOR | awk -F';' '{ print $3 }')
    if [ $i != 0 ]; then
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
    i=$(echo $VALIDATOR | awk -F';' '{ print $1 }')
    KEY=$(echo $VALIDATOR | awk -F';' '{ print $2 }')
    if [ $i != 0 ]; then
      printf ",
"
    fi
    cat <<EOF
  {
    "action": [ "Add" ],
    "address": "$KEY"
EOF
    printf "  }"
  done
  echo ""
  echo "]"
}

create_new_validators() {
  message "Creating validator identities"
  for i in ${VALIDATORS[@]}; do
    FOLDER="$DATA_DIRECTORY/$i"
    mkdir -p $FOLDER

    sidecli setup-identity $FOLDER --uri "http://localhost:444$i"
    KEY=$(sidecli self $FOLDER | grep "key:" | awk '{ print $2 }')
    ADDRESS=$(sidecli self $FOLDER | grep "address:" | awk '{ print $2 }')
    URI=$(sidecli self $FOLDER | grep "uri:" | awk '{ print $2 }')
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
    for VALIDATOR in ${VALIDATORS[@]}; do
      KEY=$(echo $VALIDATOR | awk -F';' '{ print $2 }')
      echo "      (\"$KEY\": key);"
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

  consensus="./tezos_interop/consensus.mligo"
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

  for VALIDATOR in ${VALIDATORS[@]}; do
    i=$(echo $VALIDATOR | awk -F';' '{ print $1 }')
    FOLDER="$DATA_DIRECTORY/$i"
    validators_json >"$FOLDER/validators.json"
    trusted_validator_membership_change_json >"$FOLDER/trusted-validator-membership-change.json"
  done

  message "Getting contract address"
  TEZOS_CONSENSUS_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract consensus | grep KT1 | tr -d '\r')"

  message "Configuring Deku nodes"
  for VALIDATOR in ${VALIDATORS[@]}; do
    i=$(echo $VALIDATOR | awk -F';' '{ print $1 }')
    FOLDER="$DATA_DIRECTORY/$i"

    sidecli setup-tezos "$FOLDER" \
      --tezos_consensus_contract="$TEZOS_CONSENSUS_ADDRESS" \
      --tezos_rpc_node=$RPC_NODE \
      --tezos_secret="$SECRET_KEY" \
      --unsafe_tezos_required_confirmations 1
  done
}

stop() {
  if [[ $(docker ps | grep my-sandbox) ]]; then
    docker kill my-sandbox
    rm -rf "$DATA_DIRECTORY/**"
    echo "Stopped the sandbox and wiped all state."
  fi
}

start_node() {
  stop
  message "Starting sandbox"
  docker run --rm --name my-sandbox --detach -p 20000:20000 \
    tqtezos/flextesa:20210602 granabox start
  sleep 3
  message "Sandbox started"
  message "Configuring Tezos client"
  tezos-client --endpoint $RPC_NODE bootstrapped
  tezos-client --endpoint $RPC_NODE config update
  tezos-client --endpoint $RPC_NODE import secret key myWallet "unencrypted:$SECRET_KEY" --force
}

help() {
  # FIXME: fix these docs
  echo "$0 automates deployment of a Tezos testnet node and setup of a Deku cluster."
  echo ""
  echo "Usage: $0 start|stop"
  echo "Commands:"
  echo "start"
  echo "  Does the following:"
  echo "  - Starts a node if one is not already running."
  echo "  - Imports a new snapshot if the current one is older than 1 day."
  echo "  - Creates a wallet called 'myWallet' if one doesn't exist already."
  echo "  - Adds funds to 'myWallet' if it needs them."
  echo "  - Generates new validator identities."
  echo "  - Deploys a new contract to the Tezos testnet configured to use these validators."
  echo "stop"
  echo "  Stops the Tezos node"
}

case "$1" in
start)
  start_node
  create_new_validators
  message "Warning"
  echo "This script creates a sandbox node and is for development purposes only."
  echo "It does unsafe things like lowering the required Tezos confirmations to limits unreasonable for production."
  message "Do not use these settings in production!"
  ;;
stop)
  stop
  ;;
*)
  help
  ;;
esac
