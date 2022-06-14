#! /usr/bin/env bash

set -e

GREEN='\033[0;32m'
NC='\033[0m'

function print_info { printf "=== %s ===\n" "$1"; }
function print_success { printf "=== ${GREEN}%s${NC} ===\n" "$1"; }

function step {
  printf "%s\n" "$1"
  printf "Continue? [Yn] "
  read -r action
  if [ "$action" == "n" ]; then exit 2; fi
  if [ "$action" == "N" ]; then exit 2; fi
}

function create_wallet {
  WALLET_NAME=${1:-"deku-testnet"}
  RPC_NODE=${2:-"https://ithacanet.tezos.marigold.dev"}

  # Generate a new key pair, we are not forcing here as to not override the current key
  tezos-client -E "$RPC_NODE" gen keys "$KEY_NAME" || true

  # Get the address
  PUBLIC_KEY_HASH=$(tezos-client -E "$RPC_NODE" show address "$WALLET_NAME" | grep 'Hash:' | awk '{ print $2}')

  curl "https://faucet-bot.marigold.dev/ithaca/getmoney/$PUBLIC_KEY_HASH"
}

print_info "Making sure ./data exists"
mkdir -p ./data

# number of nodes to generate, starting from 0
NUMBER_OF_NODES=${1:-"3"}

# create a array of numbers for looping later
# https://github.com/koalaman/shellcheck/wiki/SC2207
# shellcheck disable=SC2207
VALIDATORS=( $(seq 0 "$((NUMBER_OF_NODES - 1))") )

# base name to use
KEY_NAME=${2:-"deku-testnet"}

# tezos RPC node
RPC_NODE=${3:-"https://ithacanet.tezos.marigold.dev"}

# contract names
CONSENSUS_NAME="$KEY_NAME-consensus"
DISCOVERY_NAME="$KEY_NAME-discovery"


print_info "Creating a wallet for the contracts"
create_wallet "$KEY_NAME"
print_success "Wallet created and got money"

print_info "Generating identitites"
for VALIDATOR in "${VALIDATORS[@]}"; do
  print_info "Creating a wallet for the node"
  create_wallet "$KEY_NAME-$VALIDATOR"
  print_success "Wallet created and got money"
  echo "peer-$VALIDATOR.$KEY_NAME.gcp-npr.marigold.dev"
  nix run github:marigold-dev/deku#deku-cli -- setup-identity "data/$VALIDATOR"  --uri="peer-$VALIDATOR.$KEY_NAME.gcp-npr.marigold.dev"
done
print_success "Identities generated"

step "Check in ./data to make sure everything looks OK, you might want to change the uri"

# read hashes from generated identities
# shellcheck disable=2038
DEKU_ADDRESSES=$(find data -iname identity.json | xargs jq '.t' -r | tr '\n' ' ')

print_info "Creating consensus contract"
# create the consensus contract using the addresses from above
# This is provided from the parent script
# We are expecting it to split the words here so we disable shellcheck
# shellcheck disable=2086
create-consensus-contract $DEKU_ADDRESSES

step "Consensus contract created for $DEKU_ADDRESSES, next step is originating it, "

print_info "Deploying consensus contract"
# deploy the consensus contract to the network
tezos-client --endpoint "$RPC_NODE" originate contract "$CONSENSUS_NAME" \
  transferring 0 from "$KEY_NAME" \
  running "$(cat ./consensus_contract)" \
  --init "$(cat ./consensus_storage)" \
  --burn-cap 2 \
  --force

rm ./consensus_contract ./consensus_storage

print_success "Consensus contract deployed"

# read public key and uri from the generated identity.json files
# shellcheck disable=2038
TEZOS_ADDRESSES=$(find data -iname identity.json | xargs jq '. | "\(.t),\(.uri)"' -r | tr '\n' ' ')

print_info "Creating discovery contract"
# create discovery contract with public keys and uris from above
# This is provided from the parent script
# We are expecting it to split the words here so we disable shellcheck
# shellcheck disable=2086
create-discovery-contract $TEZOS_ADDRESSES

step "Discovery contract created for $TEZOS_ADDRESSES, next step is originating it, "

print_info "Deploying discovery contract"
# deploy the discovery contract
tezos-client --endpoint "$RPC_NODE" originate contract "$DISCOVERY_NAME" \
  transferring 0 from "$KEY_NAME" \
  running "$(cat ./discovery_contract)" \
  --init "$(cat ./discovery_storage)" \
  --burn-cap 2 \
  --force

rm ./discovery_contract ./discovery_storage

print_success "Consensus discovery deployed"

print_info "Getting contract addresses"
# Get contract data
TEZOS_CONSENSUS_ADDRESS="$(tezos-client --endpoint "$RPC_NODE" show known contract "$CONSENSUS_NAME" | grep KT1 | tr -d '\r')"
TEZOS_DISCOVERY_ADDRESS="$(tezos-client --endpoint "$RPC_NODE" show known contract "$DISCOVERY_NAME" | grep KT1 | tr -d '\r')"
print_success "Got contract addresses"

print_info "Generating tezos.json for nodes"
# generate tezos.json
for VALIDATOR in "${VALIDATORS[@]}"; do
  # Get private key for the specific node
  TEZOS_PRIVATE_KEY=$(tezos-client -E "$RPC_NODE" show address "$KEY_NAME-$VALIDATOR" --show-secret | grep 'Secret Key:' | awk '{ print $3}' | sed s/unencrypted://)

  echo '{"rpc_node": "", "secret": "", "consensus_contract": "", "discovery_contract": "", "required_confirmations": 10}' \
    | jq '.rpc_node = $rpc_node' --arg rpc_node "$RPC_NODE" \
    | jq '.secret = $secret' --arg secret "$TEZOS_PRIVATE_KEY" \
    | jq '.consensus_contract = $consensus_contract' --arg consensus_contract "$TEZOS_CONSENSUS_ADDRESS" \
    | jq '.discovery_contract = $discovery_contract' --arg discovery_contract "$TEZOS_DISCOVERY_ADDRESS" > "data/$VALIDATOR/tezos.json"
done
print_success "tezos.json files generated"

echo "Make sure the ./data directory looks like what you expect"