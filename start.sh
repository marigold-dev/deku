#!/usr/bin/env bash

kill() {
    echo "Caught exit. Killing all deku nodes"
    killall deku-node
}

trap kill EXIT

for N in 0 1 2 3
do
    dune exec deku-node -- -p "444$N" -s ./chain/data/$N/storage.json |& awk -v n=$N '{ print "node " n ": " $0}' &
    sleep 0.1
done


export DEKU_BOOTSTRAP_SECRET="edsk2jS9cPs3Tgp6T34NQvPPPUPGYtGysCHS569FgDAKmdkKutQHUw"
export DEKU_VALIDATOR_URIS="http://localhost:4440,http://localhost:4441,http://localhost:4442,http://localhost:4443"
export DEKU_VALIDATORS="tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK,tz1PYdVbnLwiqKo3fLFXTKxw6K7BhpddQPh8,tz1Pv4viWq7ye4R6cr9SKR3tXiZGvpK34SKi,tz1cXKCCxLwYCHDSrx9hfD5Qmbs4W8w2UKDw"
(cd chain && dune exec deku-bootstrap)

wait
