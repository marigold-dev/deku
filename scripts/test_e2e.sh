#! /usr/bin/env bash

# FIXME: wallets/corresponding pairs of tz1 & secrets

export DEKU_TEZOS_RPC_NODE="http://localhost:20000"

# used by .ml programs as well
export DEKU_VERBOSE_TESTS=${DEKU_VERBOSE_TESTS:-"false"}

dir=$(dirname $0)
source $dir/common.sh

read -r tz_addr1 secret1<<<$(line_nth 3 $dir/identities.txt)
read -r tz_addr2 secret2<<<$(line_nth 4 $dir/identities.txt)

$dir/deploy_contracts.sh

test-failed() {
  echo "!!! Test failed !!!"
  killall deku-node
  exit 1
}

# Takes a raw proof and runs it on Tezos
tezos-withdraw() {
  id=$(proof_id "$1")
  handle_hash=$(handle_hash "$1")
  proof=$(proof "$1")

  if ! tezos_output="$(tezos_client transfer 0 from bob to dummy_ticket \
      --entrypoint withdraw_from_deku \
      --arg "Pair (Pair \"$DEKU_TEZOS_CONSENSUS_ADDRESS\" (Pair (Pair (Pair 10 0x) \
        (Pair $id \"$DUMMY_TICKET_ADDRESS\")) \"$DUMMY_TICKET_ADDRESS\")) (Pair $handle_hash $proof)" \
      --burn-cap 2)"
  then
    echo Withdraw failed on Tezos:
    echo "$tezos_output"
    test-failed
  fi
}

message "Starting the nodes"

# Copied from start.sh
export DEKU_BOOTSTRAP_KEY="edpku8312JdFovNcX9AkFiafkAcVCHDvAe3zBTvF4YMyLEPz4KFFMd"
export DEKU_VALIDATORS="tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK,tz1PYdVbnLwiqKo3fLFXTKxw6K7BhpddQPh8,tz1Pv4viWq7ye4R6cr9SKR3tXiZGvpK34SKi,tz1cXKCCxLwYCHDSrx9hfD5Qmbs4W8w2UKDw"
export DEKU_VALIDATOR_URIS="http://localhost:4440,http://localhost:4441,http://localhost:4442,http://localhost:4443"
export DEKU_TEZOS_SECRET="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"
export DEKU_TEZOS_CONSENSUS_ADDRESS="$(tezos_client --endpoint $DEKU_TEZOS_RPC_NODE show known contract consensus | grep KT1 | tr -d '\r')"
export DEKU_TEZOS_DISCOVERY_ADDRESS="$(tezos_client --endpoint http://localhost:20000 show known contract discovery | grep KT1 | tr -d '\r')"
export DUMMY_TICKET_ADDRESS="$(tezos_client --endpoint $DEKU_TEZOS_RPC_NODE show known contract dummy_ticket | grep KT1 | tr -d '\r')"

if [ "$DEKU_VERBOSE_TESTS" ] && [ "$DEKU_VERBOSE_TESTS" != "false" ]
then
  stdout=/dev/stdout
else
  stdout=/dev/null
fi

for N in 0 1 2 3; do
  source "./chain/data/$N/env"
  _build/install/default/bin/deku-node \
    --port "444$N" \
    --database-uri "sqlite3:./chain/data/$N/database.db" |&
    awk -v n=$N '{ print "node " n ": " $0}' > $stdout &  # TODO silence as an option
  sleep 0.1
done

export DEKU_BOOTSTRAP_SECRET="edsk2jS9cPs3Tgp6T34NQvPPPUPGYtGysCHS569FgDAKmdkKutQHUw"

_build/install/default/bin/deku-bootstrap

sleep 5

message "Doing the deposit"

tezos_client transfer 0 from bob to dummy_ticket \
  --entrypoint mint_to_deku --arg "Pair (Pair \"$DEKU_TEZOS_CONSENSUS_ADDRESS\" \"$tz_addr1\") (Pair 100 0x)" \
  --burn-cap 2

ticket_data="Pair \"$DUMMY_TICKET_ADDRESS\" 0x"

sleep 5

# We do 3 withdraws:
# Withdraw 1 proof should not change (except for the tree)
# Withdraw 2 should succeed
# Withdraw 3 should fail
# 2 and 3 proofs are also reversed

message Withdraw 1
op_hash1="$(dune exec src/tezos_interop/tests/withdraw_test.exe "$ticket_data" $DUMMY_TICKET_ADDRESS $secret1 | sed -n 's/operation.hash: "\(Do[[:alnum:]]*\)"/\1/p')"

message Transfer to $tz_addr2

dune exec src/tezos_interop/tests/transaction_test.exe "$ticket_data" $tz_addr2 $secret1

sleep 3

message Proof of withdraw 1
proof1_1=$(dune exec src/tezos_interop/tests/proof_test.exe $op_hash1)

message Withdraw 2
op_hash2="$(dune exec src/tezos_interop/tests/withdraw_test.exe "$ticket_data" $DUMMY_TICKET_ADDRESS $secret2 | sed -n 's/operation.hash: "\(Do[[:alnum:]]*\)"/\1/p')" || exit 1

sleep 2

message Withdraw 3
op_hash3="$(dune exec src/tezos_interop/tests/withdraw_test.exe "$ticket_data" $DUMMY_TICKET_ADDRESS $secret2 | sed -n 's/operation.hash: "\(Do[[:alnum:]]*\)"/\1/p')" || exit 1

sleep 5 # FIXME check how long we have to wait

message Proof of withdraw 3
proof3_1=$(dune exec src/tezos_interop/tests/proof_test.exe $op_hash3)

message Proof of withdraw 2
proof2_1=$(dune exec src/tezos_interop/tests/proof_test.exe $op_hash2)

message Proof of withdraw 1, again
proof1_2=$(dune exec src/tezos_interop/tests/proof_test.exe $op_hash1)

sleep 10

if [ "$proof3_1" ]
then
  echo "Withdraw 3 did NOT fail"
  test-failed
fi

if [ -z "$proof1_1" ] || [ -z "$proof1_2" ] || [ -z "$proof2_1" ]
then
  echo "One of the correct withdraws failed"
  test-failed
fi

if [ $(proof_id "$proof1_1") != $(proof_id "$proof1_2") ]
then
  echo "Proof IDs should be the same for the following proofs:"
  echo $proof1_1
  echo $proof1_2
  test-failed
fi

tezos-withdraw "$proof1_2"
message "First withdraw succeded on Tezos"
tezos-withdraw "$proof2_1"
message "Second withdraw succeded on Tezos"

message "Test succeeded"
killall deku-node
