#! /usr/bin/env bash

set -e

data_directory="data"

[ "$USE_NIX" ] || export LD_LIBRARY_PATH=$(esy x sh -c 'echo $LD_LIBRARY_PATH')
[ "$USE_NIX" ] || export PATH=$(esy x sh -c 'echo $PATH')

tezos-client() {
  docker exec -it deku_flextesa tezos-client "$@"
}

ligo() {
  docker run --rm -v "$PWD":"$PWD" -w "$PWD" ligolang/ligo:0.27.0 "$@"
}

RPC_NODE=http://localhost:20000

# This secret key never changes.
SECRET_KEY="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"

DATA_DIRECTORY="data"

VALIDATORS=(0 1 2)

message() {
  echo "=========== $@ ==========="
}

deploy_ctez() {
  # Modified version of ./ctez/deploy.sh

  DEPLOYMENT_DATE=$(date '+%Y-%m-%d')

  # Build and deploy ctez
  contract=$(ligo compile contract ./ctez/ctez.mligo)
  storage=$(ligo compile storage ./ctez/ctez.mligo "$(sed s/DEPLOYMENT_DATE/${DEPLOYMENT_DATE}/ <./ctez/ctez_initial_storage.mligo)")
  tezos-client --endpoint $RPC_NODE originate contract "ctez" \
    transferring 0 from myWallet \
    running "$contract" \
    --init "$storage" \
    --burn-cap 10 \
    --force
  CTEZ_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract ctez | grep KT1 | tr -d '\r')"

  # Build and deploy the fa12 for ctez
  contract=$(ligo compile contract ./ctez/fa12.mligo)
  storage=$(ligo compile storage ./ctez/fa12.mligo "$(sed s/ADMIN_ADDRESS/$CTEZ_ADDRESS/ <./ctez/fa12_ctez_initial_storage.mligo)")
  tezos-client --endpoint $RPC_NODE originate contract "fa12_ctez" \
    transferring 0 from myWallet \
    running "$contract" \
    --init "$storage" \
    --burn-cap 10 \
    --force
  FA12_CTEZ_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract fa12_ctez | grep KT1 | tr -d '\r')"

  # Build and deploy cfmm
  contract=$(ligo compile contract ./ctez/cfmm_tez_ctez.mligo)
  sed s/FA12_CTEZ/${FA12_CTEZ_ADDRESS}/ <./ctez/cfmm_initial_storage.mligo | sed s/CTEZ_ADDRESS/${CTEZ_ADDRESS}/ >_build/cfmm_storage.mligo

  storage=$(ligo compile storage ./ctez/cfmm_tez_ctez.mligo "$(<_build/cfmm_storage.mligo)")
  tezos-client --endpoint $RPC_NODE originate contract "cfmm" \
    transferring 0.000001 from myWallet \
    running "$contract" \
    --init "$storage" \
    --burn-cap 10 \
    --force
  CFMM_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract cfmm | grep KT1 | tr -d '\r')"

  # Build and deploy the fa12 for the cfmm lqt, specifying the cfmm as admin
  contract=$(ligo compile contract ./ctez/fa12.mligo)
  storage=$(ligo compile storage ./ctez/fa12.mligo "$(sed s/ADMIN_ADDRESS/$CFMM_ADDRESS/ <./ctez/fa12_ctez_initial_storage.mligo)")
  tezos-client --endpoint $RPC_NODE originate contract "fa12_lqt" \
    transferring 0 from myWallet \
    running "$contract" \
    --init "$storage" \
    --burn-cap 10 \
    --force
  FA12_LQT_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract fa12_lqt | grep KT1 | tr -d '\r')"

  # Set the lqt fa12 address in the cfmm
  tezos-client --endpoint $RPC_NODE transfer 0 from myWallet to cfmm \
    --entrypoint setLqtAddress --arg "\"$FA12_LQT_ADDRESS\"" \
    --burn-cap 10

  # Set the ctez fa12 address and the cfmm address in the oven management contract
  tezos-client --endpoint $RPC_NODE transfer 0 from myWallet to ctez \
    --entrypoint set_addresses --arg "Pair \"$CFMM_ADDRESS\" \"$FA12_CTEZ_ADDRESS\"" \
    --burn-cap 10

  message finishe
}

deploy_fa12_to_ticket() {
  contract=$(ligo compile contract ./src/tezos_interop/fa12_ticket_wrapper.religo)

  message "Originating FA1.2 Ticket Wrapper contract"
  sleep 2
  tezos-client --endpoint $RPC_NODE originate contract "fa12_ticket_wrapper" \
    transferring 0 from myWallet \
    running "$contract" \
    --init "Unit" \
    --burn-cap 2 \
    --force
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
    ADDRESS=$(echo $VALIDATOR | awk -F';' '{ print $4 }')
    if [ $i != 0 ]; then
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
  for i in ${VALIDATORS[@]}; do
    FOLDER="$DATA_DIRECTORY/$i"
    mkdir -p $FOLDER

    deku-cli setup-identity $FOLDER --uri "http://localhost:444$i"
    KEY=$(deku-cli self $FOLDER | grep "key:" | awk '{ print $2 }')
    ADDRESS=$(deku-cli self $FOLDER | grep "address:" | awk '{ print $2 }')
    URI=$(deku-cli self $FOLDER | grep "uri:" | awk '{ print $2 }')
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
      ADDRESS=$(echo $VALIDATOR | awk -F';' '{ print $4 }')
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

    deku-cli setup-tezos "$FOLDER" \
      --tezos_consensus_contract="$TEZOS_CONSENSUS_ADDRESS" \
      --tezos_rpc_node=$RPC_NODE \
      --tezos_secret="$SECRET_KEY" \
      --unsafe_tezos_required_confirmations 1
  done
  echo "Tezos Contract address: $TEZOS_CONSENSUS_ADDRESS"
}

tear-down() {
  for i in ${VALIDATORS[@]}; do
    FOLDER="$DATA_DIRECTORY/$i"
    if [ -d $FOLDER ]; then
      rm -r $FOLDER
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

start_deku_cluster() {
  SERVERS=()
  echo "Starting nodes."
  for i in ${VALIDATORS[@]}; do
    deku-node "$data_directory/$i" &
    SERVERS+=($!)
  done

  sleep 1

  echo "Producing a block"
  HASH=$(deku-cli produce-block "$data_directory/0" | awk '{ print $2 }')

  sleep 0.1

  echo "Signing"
  for i in ${VALIDATORS[@]}; do
    deku-cli sign-block "$data_directory/$i" $HASH
  done

  for PID in ${SERVERS[@]}; do
    wait $PID
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

}

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
  ;;
tear-down)
  tear-down
  ;;
deploy-ticket)
  start_tezos_node
  # deploy_ctez
  deploy_fa12_to_ticket
  ;;
*)
  help
  ;;
esac
