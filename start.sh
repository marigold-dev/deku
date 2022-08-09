#!/usr/bin/env bash

tezos-client() {
  docker exec -t deku_flextesa tezos-client "$@"
}


trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

for N in 0 1 2 3
do
    dune exec deku-node -- \
        -p "444$N" \
        -s ./chain/data/$N/storage.json \
        -d "sqlite3:./chain/data/$N/database.db" \
        |& awk -v n=$N '{ print "node " n ": " $0}' &
    sleep 0.1
done


export DEKU_BOOTSTRAP_SECRET="edsk2jS9cPs3Tgp6T34NQvPPPUPGYtGysCHS569FgDAKmdkKutQHUw"
export DEKU_VALIDATOR_URIS="http://localhost:4440,http://localhost:4441,http://localhost:4442,http://localhost:4443"
export DEKU_VALIDATORS="tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK,tz1PYdVbnLwiqKo3fLFXTKxw6K7BhpddQPh8,tz1Pv4viWq7ye4R6cr9SKR3tXiZGvpK34SKi,tz1cXKCCxLwYCHDSrx9hfD5Qmbs4W8w2UKDw"
export DEKU_CONSENSUS_CONTRACT="$(tezos-client --endpoint http://localhost:20000 show known contract consensus | grep KT1 | tr -d '\r')"
export DEKU_DISCOVERY_CONTRACT="$(tezos-client --endpoint http://localhost:20000 show known contract discovery | grep KT1 | tr -d '\r')"
export DEKU_TEZOS_SECRET="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"

(cd chain && dune exec deku-bootstrap)

wait
