#! /bin/sh

set -e 
data_directory="data"

SIDECLI=$(esy x which sidecli)
sidecli () {
  eval $SIDECLI '"$@"'
}

HTTP_SERVER=$(esy x which http_server)
http_server () {
  eval $HTTP_SERVER '"$@"'
}

VALIDATORS=(0 1 2)
SERVERS=()
echo "Starting nodes."
for i in ${VALIDATORS[@]}; do
  http_server "$data_directory/$i" &
  SERVERS+=($!)
done

sleep 0.1

echo "Producing a block"
HASH=$(sidecli produce-block "$data_directory/0" | awk '{ print $2 }')

sleep 0.1

echo "Signing"
for i in ${VALIDATORS[@]}; do
  sidecli sign-block "$data_directory/$i" $HASH
done

for PID in ${SERVERS[@]}; do
  wait $PID
done
