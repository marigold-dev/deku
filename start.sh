#! /usr/bin/env bash

set -e 
data_directory="data"

deku_cli () {
  eval dune exec src/bin/deku_cli.exe -- '"$@"'
}

deku_node () {
  ./_build/default/src/bin/deku_node.exe "$@"
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
