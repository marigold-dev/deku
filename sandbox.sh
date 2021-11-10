#! /bin/sh

set -e
data_directory="data"

SIDECLI=$(esy x which sidecli)
sidecli() {
  eval $SIDECLI '"$@"'
}

VALIDATORS=(0 1 2)
echo "Creating validator identities."
for i in ${VALIDATORS[@]}; do
  FOLDER="$data_directory/$i"
  mkdir -p $FOLDER

  sidecli setup-identity $FOLDER --uri "http://localhost:444$i"
  KEY=$(sidecli self $FOLDER | grep "key:" | awk '{ print $2 }')
  ADDRESS=$(sidecli self $FOLDER | grep "key:" | awk '{ print $2 }')
  URI=$(sidecli self $FOLDER | grep "uri:" | awk '{ print $2 }')
  VALIDATORS[$i]="$i;$KEY;$URI;$ADDRESS"
done

# To register the validators, run consensus.mligo with the list of
# validators. To do this quickly, open the LIGO IDE with the url
# provided and paste the following storage as inputs to the contract.
echo "Paste the following in the storage section on the LIGO playground at https://ide.ligolang.org/p/-x6CdYJ5tIEaVzD9lGYsaA and note the address of the contract"
echo ""
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
echo ""

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

for VALIDATOR in ${VALIDATORS[@]}; do
  i=$(echo $VALIDATOR | awk -F';' '{ print $1 }')
  FOLDER="$data_directory/$i"

  validators_json >"$FOLDER/validators.json"
  trusted_validator_membership_change_json >"$FOLDER/trusted-validator-membership-change.json"
done

# With the validators registered, use the address of the originated
# contract in the command to setup the individual nodes
read -p "Enter address of the deployed contract: " TEZOS_CONSENSUS_ADDRESS
read -p "Enter the URI to your Tezos RPC: " TEZOS_RPC_NODE
read -p "Enter the secret key of a wallet: " TEZOS_SECRET

for VALIDATOR in ${VALIDATORS[@]}; do
  i=$(echo $VALIDATOR | awk -F';' '{ print $1 }')
  FOLDER="$data_directory/$i"

  sidecli setup-tezos "$FOLDER" \
    --tezos_consensus_contract="$TEZOS_CONSENSUS_ADDRESS" \
    --tezos_rpc_node="$TEZOS_RPC_NODE" \
    --tezos_secret="$TEZOS_SECRET"
done
