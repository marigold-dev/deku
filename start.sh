#!/usr/bin/env bash

trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

for N in 0 1 2 3
do
    dune exec deku-node -- -p "444$N" -s ./chain/data/$N/storage.json |& awk -v n=$N '{ print "node " n ": " $0}' &
    sleep 0.1
done

(cd chain && dune exec deku-bootstrap -- bootstrap)

wait
