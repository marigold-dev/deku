#! /usr/bin/env bash

# FIXME: throw this script too - we don't want more scripts

export DEKU_TEZOS_RPC_NODE="http://localhost:20000"

dir=$(dirname $0)
source $dir/common.sh

echo "*** Starting the nodes ***"

# Copied from start.sh
export DEKU_BOOTSTRAP_KEY="edpku8312JdFovNcX9AkFiafkAcVCHDvAe3zBTvF4YMyLEPz4KFFMd"
export DEKU_VALIDATORS="tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK,tz1PYdVbnLwiqKo3fLFXTKxw6K7BhpddQPh8,tz1Pv4viWq7ye4R6cr9SKR3tXiZGvpK34SKi,tz1cXKCCxLwYCHDSrx9hfD5Qmbs4W8w2UKDw"
export DEKU_VALIDATOR_URIS="http://localhost:4440,http://localhost:4441,http://localhost:4442,http://localhost:4443"
export DEKU_TEZOS_SECRET="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"
export DEKU_TEZOS_CONSENSUS_ADDRESS="$(tezos-client --endpoint $DEKU_TEZOS_RPC_NODE show known contract consensus | grep KT1 | tr -d '\r')"

for N in 0 1 2 3; do
  source "./chain/data/$N/env"
  _build/install/default/bin/deku-node \
    --port "444$N" \
    --database-uri "sqlite3:./chain/data/$N/database.db" |&
    awk -v n=$N '{ print "node " n ": " $0}' > /dev/null &
  sleep 0.1
done

export DEKU_BOOTSTRAP_SECRET="edsk2jS9cPs3Tgp6T34NQvPPPUPGYtGysCHS569FgDAKmdkKutQHUw"

_build/install/default/bin/deku-bootstrap

tz1_address="tz1Uodi1AwskSfdHiRUBh1Z856fMrSXCpc5i"
ticket_address="$(tezos-client --endpoint $DEKU_TEZOS_RPC_NODE show known contract dummy_ticket | grep KT1 | tr -d '\r')"

sleep 5

message Initial deposit

tezos-client transfer 0 from alice to dummy_ticket \
  --entrypoint mint_to_deku --arg "Pair (Pair \"$DEKU_TEZOS_CONSENSUS_ADDRESS\" \"$tz1_address\") (Pair 100 0x)" \
  --burn-cap 2

sleep 5

echo Withdraw 1
op_hash1="$(dune exec src/tezos_interop/tests/withdraw_test.exe $ticket_address | sed -n 's/operation.hash: "\(Do[[:alnum:]]*\)"/\1/p')"

sleep 3

echo Proof of withdraw 1
proof1_1=$(dune exec src/tezos_interop/tests/proof_test.exe $op_hash1)
dune exec src/tezos_interop/tests/proof_test.exe $op_hash1

echo Withdraw 2
op_hash2="$(dune exec src/tezos_interop/tests/withdraw_test.exe $ticket_address | sed -n 's/operation.hash: "\(Do[[:alnum:]]*\)"/\1/p')" || exit 1

sleep 2

echo Withdraw 3
op_hash3="$(dune exec src/tezos_interop/tests/withdraw_test.exe $ticket_address | sed -n 's/operation.hash: "\(Do[[:alnum:]]*\)"/\1/p')" || exit 1

sleep 3 # FIXME check how long we have to wait

# Request proofs out of order on purpose
# but TODO save and test the result
dune exec src/tezos_interop/tests/proof_test.exe $op_hash3 || exit 1

dune exec src/tezos_interop/tests/proof_test.exe $op_hash2 || exit 1

dune exec src/tezos_interop/tests/proof_test.exe $op_hash1 || exit 1

echo
echo "Please check that those proofs make sense"
