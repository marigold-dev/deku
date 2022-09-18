#!/usr/bin/env bash

trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

dune build

tezos-client() {
  docker exec -t deku_flextesa tezos-client "$@"
}

export DEKU_BOOTSTRAP_KEY="edpku8312JdFovNcX9AkFiafkAcVCHDvAe3zBTvF4YMyLEPz4KFFMd"
export DEKU_VALIDATORS="tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK,tz1PYdVbnLwiqKo3fLFXTKxw6K7BhpddQPh8,tz1Pv4viWq7ye4R6cr9SKR3tXiZGvpK34SKi,tz1cXKCCxLwYCHDSrx9hfD5Qmbs4W8w2UKDw"
export DEKU_VALIDATOR_URIS="http://localhost:4440,http://localhost:4441,http://localhost:4442,http://localhost:4443"

for N in 0 1 2 3; do
  source "./chain/data/$N/env"
  _build/install/default/bin/deku-node \
    --port "444$N" |&
    awk -v n=$N '{ print "node " n ": " $0}' &
  sleep 0.1
done

export DEKU_BOOTSTRAP_SECRET="edsk2jS9cPs3Tgp6T34NQvPPPUPGYtGysCHS569FgDAKmdkKutQHUw"

sleep 3

_build/install/default/bin/deku-bootstrap

wait
