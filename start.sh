#! /bin/bash

set -e 
data_directory="data"

LD_LIBRARY_PATH=$(esy x sh -c 'echo $LD_LIBRARY_PATH')
export LD_LIBRARY_PATH

DEKU_CLI=$(esy x which deku-cli)
deku_cli () {
  eval $DEKU_CLI '"$@"'
}

# Start a deku-node
DEKU_NODE=$(esy x which deku-node)
deku_node () {
  eval $DEKU_NODE '"$@"'
}

VALIDATORS=(0 1 2)
SERVERS=()
echo -e "\e[35m\e[1m**************************    STARTING NODES    ********************************\e[0m"

# The purpose of this loop is simply to start 3 deku-nodes
# respectively using data/0, data/1 and data/2 folders
for i in ${VALIDATORS[@]}; do
  deku_node "$data_directory/$i" &
  SERVERS+=($!)
  echo -e "\e[32m\e[1m### Started node: $i ###\e[0m"
done

sleep 1

echo -e "\e[35m\e[1m*************************    PRODUCING A BLOCK    ******************************\e[0m"
HASH=$(deku_cli produce-block "$data_directory/0" | awk '{ print $2 }')

sleep 0.1

echo -e "\e[35m\e[1m*************************       SIGNING       **********************************\e[0m"
for i in ${VALIDATORS[@]}; do
  deku_cli sign-block "$data_directory/$i" $HASH
done

for PID in ${SERVERS[@]}; do
  wait $PID
done
