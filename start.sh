#! /bin/bash

set -e 
data_directory="data"

LD_LIBRARY_PATH=$(esy x sh -c 'echo $LD_LIBRARY_PATH')
export LD_LIBRARY_PATH

DEKU_CLI=$(esy x which deku-cli)
deku_cli () {
  eval $DEKU_CLI '"$@"'
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
HASH=$(deku_cli produce-block "$data_directory/0" | awk '{ print $2 }')

sleep 0.1

echo "Signing"
for i in ${VALIDATORS[@]}; do
  deku_cli sign-block "$data_directory/$i" $HASH
done

for PID in ${SERVERS[@]}; do
  wait $PID
done
