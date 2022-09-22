#!/usr/bin/env bash

trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

dune build

tezos-client() {
  docker exec -t deku_flextesa tezos-client "$@"
}

for i in 0 1 2 3
do
  rm chain/data/$i/chain.json
  rm chain/data/$i/database.db
done

# Rebuild the sdk
cd sdks/typescript-sdk
echo $PWD
npm i > /dev/null
npm run build > /dev/null
cd ../../
echo $PWD

# Rebuild the library
cd examples/cookie-game/
echo $PWD
npm i > /dev/null
npm run build > /dev/null
cd ../../
echo $PWD

export DEKU_VALIDATORS="tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK,tz1PYdVbnLwiqKo3fLFXTKxw6K7BhpddQPh8,tz1Pv4viWq7ye4R6cr9SKR3tXiZGvpK34SKi,tz1cXKCCxLwYCHDSrx9hfD5Qmbs4W8w2UKDw"
export DEKU_VALIDATOR_URIS="http://localhost:4440,http://localhost:4441,http://localhost:4442,http://localhost:4443"
export DEKU_TEZOS_SECRET="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"
export DEKU_TEZOS_RPC_NODE="http://localhost:20000"
export DEKU_TEZOS_CONSENSUS_ADDRESS="$(tezos-client --endpoint http://localhost:20000 show known contract consensus | grep KT1 | tr -d '\r')"
export DEKU_TEZOS_DISCOVERY_ADDRESS="$(tezos-client --endpoint http://localhost:20000 show known contract discovery | grep KT1 | tr -d '\r')"
export DEKU_API_PORT=8080

for N in 0 1 2 3; do
  source "./chain/data/$N/env"

  # Creates the FIFO
  test -p "./chain/data/$N/pipe_write" || mkfifo "./chain/data/$N/pipe_write"
  test -p "./chain/data/$N/pipe_read" || mkfifo "./chain/data/$N/pipe_read"

  # Starts the VM
  node examples/cookie-game/lib/src/index.js "./chain/data/$N/pipe" |&
  awk -v n=$N '{ print "vm " n ": " $0}' &

  sleep 2

  # Starts the Node
  _build/install/default/bin/deku-node \
    --default-block-size=100 \
    --port "444$N" \
    --database-uri "sqlite3:./chain/data/$N/database.db" \
    --named-pipe-path "./chain/data/$N/pipe" \
    --data-folder "./chain/data/$N" |&
    awk -v n=$N '{ print "node " n ": " $0}' &
  sleep 0.1
done

# Only starting one API
# export DEKU_API_DEKU_NODE="http://localhost:4440"
# _build/install/default/bin/deku-api --database-uri "sqlite3:/tmp/database.db" &

wait
