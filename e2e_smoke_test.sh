#! /bin/bash

# set -e 
RPC_NODE="${RPC_NODE:-"http://localhost:20000"}"
echo "Using $RPC_NODE as RPC Node"
data_directory="data"

LD_LIBRARY_PATH=$(esy x sh -c 'echo $LD_LIBRARY_PATH')
export LD_LIBRARY_PATH

SIDECLI=$(esy x which sidecli)
sidecli () {
  eval $SIDECLI '"$@"'
}

DEKU_NODE=$(esy x which deku-node)
deku_node () {
  eval $DEKU_NODE '"$@"'
}

VALIDATORS=(0 1 2)
SERVERS=()
echo "Starting nodes."
for i in ${VALIDATORS[@]}; do
  deku_node "$data_directory/$i" &
  SERVERS+=($!)
done

sleep 1

echo "Producing a block"
HASH=$(sidecli produce-block "$data_directory/0" | awk '{ print $2 }')

sleep 0.1

echo "Signing"
for i in ${VALIDATORS[@]}; do
  sidecli sign-block "$data_directory/$i" $HASH
done

seconds=30
sleep $seconds
killall Domain0

contract=$(cat "$data_directory/0/tezos.json" | jq '.consensus_contract' | xargs)
storage=$(curl "$RPC_NODE/chains/main/blocks/head/context/contracts/$contract/storage")
current_state_hash=$(echo $storage | jq '.args[0].args[0].args[2].bytes' | xargs)
current_block_height=$(echo $storage | jq '.args[0].args[0].args[0].args[1].int' | xargs)


echo "The current block height is" $current_block_height

# Check that a state root hash was published recently
if [ $current_block_height -lt 20 ]; then
  echo "Error: no recent state root hash update found. Exiting."
  exit 1
fi

for i in ${VALIDATORS[@]}; do
  esy x asserter "$data_directory/$i" $current_state_hash $seconds
done
