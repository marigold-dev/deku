#! /usr/bin/env bash

# shellcheck source=./tilt/variables.sh
source "./tilt/variables.sh"

set -e

# =======================
# We need the tezos-client to deploy contracts.
# We will use the binary included in the Tezos testnet deployment
# See https://tezos.gitlab.io/introduction/howtouse.html#client
tezos-client() {
  docker exec -t deku_flextesa tezos-client "$@"
}

deploy_contract () {
  echo "Deploying new $1 contract"

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
  for VALIDATOR in "${NODES[@]}"; do
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

# Step 1: Set up validator identities for each node
echo "Creating validator identities"
for i in "${NODES[@]}"; do
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
  NODES[$i]="$i;$KEY;$URI;$ADDRESS"
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
  for VALIDATOR in "${NODES[@]}"; do
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

# Step 4: deploying the consensus and validators discovery contracts
consensus="./src/tezos_interop/consensus.mligo"
discovery="./src/tezos_interop/discovery.mligo"
deploy_contract "consensus" "$consensus" "$consensus_storage"
deploy_contract "discovery" "$discovery" "$(discovery_storage)"

# call validators_json() and trusted_validator_membership_change_json()
for VALIDATOR in "${NODES[@]}"; do
  i=$(echo "$VALIDATOR" | awk -F';' '{ print $1 }')
  FOLDER="$DATA_DIRECTORY/$i"
  trusted_validator_membership_change_json >"$FOLDER/trusted-validator-membership-change.json"
done

# Step 5: Look up the address of the contract we just deployed.
echo "Getting contract address"
TEZOS_CONSENSUS_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract consensus | grep KT1 | tr -d '\r')"
TEZOS_DISCOVERY_ADDRESS="$(tezos-client --endpoint $RPC_NODE show known contract discovery | grep KT1 | tr -d '\r')"

# Step 6: Finally we need to configure each Deku node to communicate with the Tezos testnet.
# This configuration is stored in a file named `tezos.json`, and is created with the `deku-cli setup-tezos``
echo "Configuring Deku nodes"
for VALIDATOR in "${NODES[@]}"; do
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