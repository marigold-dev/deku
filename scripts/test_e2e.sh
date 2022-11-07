#! /usr/bin/env bash

# used by .ml test programs as well
export DEKU_VERBOSE_TESTS=${DEKU_VERBOSE_TESTS:-"false"}

dir=$(dirname $0)
source $dir/common.sh

read -r tz_addr1 secret1 <<<$(line_nth 3 $dir/identities.txt)
read -r tz_addr2 secret2 <<<$(line_nth 4 $dir/identities.txt)

test-failed() {
  echo "!!! Test failed !!!"
  killall deku-node 2> /dev/null
  exit 1
}

# Takes a raw proof and runs it on Tezos
tezos-withdraw() {
  id=$(proof_id "$1")
  handle_hash=$(handle_hash "$1")
  proof=$(proof "$1")

  if ! tezos_output="$(octez_client --endpoint $DEKU_TEZOS_RPC_NODE transfer 0 from bob to dummy_ticket \
    --entrypoint withdraw_from_deku \
    --arg "Pair (Pair \"$DEKU_TEZOS_CONSENSUS_ADDRESS\" (Pair (Pair (Pair 10 0x) \
        (Pair $id \"$DUMMY_TICKET_ADDRESS\")) \"$DUMMY_TICKET_ADDRESS\")) (Pair $handle_hash $proof)" \
    --burn-cap 2)"; then
    echo Withdraw failed on Tezos:
    echo "$tezos_output"
    test-failed
  fi
}

public-setup() {
  if [ -z $DEKU_TEZOS_RPC_NODE ]
  then
    echo '$DEKU_TEZOS_RPC_NODE should be set to a public node (e.g. https://some.tezos.node.com)'
    exit 1
  fi
  if [ -z $DEKU_API_NODE ]   # Also used by .ml helpers
  then
    echo '$DEKU_API_NODE should be set to the public Deku node you want to use'
    exit 1
  fi

  chain_info=$(curl --silent "$DEKU_API_NODE/api/v1/chain/info")
  export DEKU_TEZOS_CONSENSUS_ADDRESS=$(echo "$chain_info" | jq -r '.consensus')
  export DEKU_TEST_MODE="kathmandu" # for commons.sh

  # We deploy our own dummy ticket for now
  source $dir/deploy_contracts.sh
  deploy_contract "dummy_ticket" \
      "./dummy_ticket.mligo" \
      "()"
      "bob"
  export DUMMY_TICKET_ADDRESS="$(octez_client --endpoint $DEKU_TEZOS_RPC_NODE show known contract dummy_ticket | grep KT1 | tr -d '\r')"
}

local-setup() {
  dune build
  export DEKU_TEZOS_RPC_NODE="http://localhost:20000"
  export DEKU_TEST_MODE=
  # API related 
  export DEKU_API_PORT=8080
  export DEKU_API_NODE="http://localhost:$DEKU_API_PORT"
  export DEKU_API_NODE_URI="127.0.0.1:4440"
  export DEKU_API_PORT=8080
  export DEKU_API_DATABASE_URI="sqlite3:/tmp/api_database.db"
  export DEKU_API_DOMAINS=8
  export DEKU_API_VM="./flextesa_chain/data/0/api_vm_pipe"
  export DEKU_API_DATA_FOLDER="./flextesa_chain/data/0/"

  message "Starting the nodes"

  trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT
  rm chain/data/*/database.db
  rm /tmp/database.db
  rm chain/data/?/*.json

  $dir/deploy_contracts.sh

  # FIXME factorize with start.sh
  # Copied from start.sh
  export DEKU_BOOTSTRAP_KEY="edpku8312JdFovNcX9AkFiafkAcVCHDvAe3zBTvF4YMyLEPz4KFFMd"
  export DEKU_VALIDATORS="tz1fpf9DffkGAnzT6UKMDoS4hZjNmoEKhGsK,tz1PYdVbnLwiqKo3fLFXTKxw6K7BhpddQPh8,tz1Pv4viWq7ye4R6cr9SKR3tXiZGvpK34SKi,tz1cXKCCxLwYCHDSrx9hfD5Qmbs4W8w2UKDw"
  export DEKU_VALIDATOR_URIS="127.0.0.1:4440,127.0.0.1:4441,127.0.0.1:4442,127.0.0.1:4443"
  export DEKU_TEZOS_SECRET="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"
  export DEKU_TEZOS_CONSENSUS_ADDRESS="$(octez_client --endpoint $DEKU_TEZOS_RPC_NODE show known contract consensus | grep KT1 | tr -d '\r')"
  export DEKU_DUMMYT_TICKET="$(octez_client --endpoint $DEKU_TEZOS_RPC_NODE show known contract dummy_ticket | grep KT1 | tr -d '\r')"
  export DEKU_API_PORT=8080
  export DEKU_DEFAULT_BLOCK_SIZE=${DEKU_DEFAULT_BLOCK_SIZE:-10000}
  echo "consensus address $DEKU_TEZOS_CONSENSUS_ADDRESS"
  export DUMMY_TICKET_ADDRESS="$(octez_client --endpoint $DEKU_TEZOS_RPC_NODE show known contract dummy_ticket | grep KT1 | tr -d '\r')"



  message "Starting the API"
  ./_build/install/default/bin/deku-api &
  sleep 3

  for N in 0 1 2 3; do
    source "./networks/flextesa/node_${N}_env"

    # Creates the FIFO
    test -p "./flextesa_chain/data/$N/pipe_write" || mkfifo "./flextesa_chain/data/$N/pipe_write"
    test -p "./flextesa_chain/data/$N/pipe_read" || mkfifo "./flextesa_chain/data/$N/pipe_read"

    sleep 2

    # Starts the Node
    _build/install/default/bin/deku-node \
      --default-block-size=10000 \
      --port "444$N" \
      --database-uri "sqlite3:./flextesa_chain/data/$N/database.db" \
      --data-folder "./flextesa_chain/data/$N" &
      #awk -v n=$N '{ print "node " n ": " $0}' &
    sleep 0.1
  done
  message "Local setup: DONE"
}

run-test() {
  echo "Debug: check the following information"
  echo "Dummy ticket address:" $DUMMY_TICKET_ADDRESS
  echo "Tezos RPC node:" $DEKU_TEZOS_RPC_NODE
  echo "Consensus address:" $DEKU_TEZOS_CONSENSUS_ADDRESS

  message "Doing the deposit"

  octez_client --endpoint $DEKU_TEZOS_RPC_NODE transfer 0 from bob to $DUMMY_TICKET_ADDRESS \
    --entrypoint mint_to_deku --arg "Pair (Pair \"$DEKU_TEZOS_CONSENSUS_ADDRESS\" \"$tz_addr1\") (Pair 100 0x)" \
    --burn-cap 2

  ticket_data="Pair \"$DUMMY_TICKET_ADDRESS\" 0x"

  sleep 20

  # We do 3 withdraws:
  # Withdraw 1 proof should not change (except for the tree)
  # Withdraw 2 should succeed
  # Withdraw 3 should fail
  # 2 and 3 proofs are also reversed

  balance=$(curl --silent "$DEKU_API_NODE/api/v1/balance/$tz_addr1/$DUMMY_TICKET_ADDRESS/0x" | jq -r '.balance')
  if (( "$balance" != 100 ))
  then
    echo "balance should be 100, but is $balance"
    test-failed
  else
    echo "Received $balance tickets"
  fi

  message Withdraw 1
  op_hash1="$(dune exec deku-p/src/core/tezos_interop/tests/withdraw_test.exe "$ticket_data" $DUMMY_TICKET_ADDRESS $secret1 | sed -n 's/operation.hash: "\(Do[[:alnum:]]*\)"/\1/p')"

  message Transfer to $tz_addr2

  dune exec deku-p/src/core/tezos_interop/tests/transaction_test.exe "$ticket_data" $tz_addr2 $secret1

  sleep 12

  message Proof of withdraw 1
  proof1_1=$(dune exec deku-p/src/core/tezos_interop/tests/proof_test.exe $op_hash1)

  message Withdraw 2
  op_hash2="$(dune exec deku-p/src/core/tezos_interop/tests/withdraw_test.exe "$ticket_data" $DUMMY_TICKET_ADDRESS $secret2 | sed -n 's/operation.hash: "\(Do[[:alnum:]]*\)"/\1/p')" || exit 1

  sleep 12

  message Withdraw 3
  op_hash3="$(dune exec deku-p/src/core/tezos_interop/tests/withdraw_test.exe "$ticket_data" $DUMMY_TICKET_ADDRESS $secret2 | sed -n 's/operation.hash: "\(Do[[:alnum:]]*\)"/\1/p')" || exit 1

  sleep 30 # FIXME check how long we have to wait

  message Proof of withdraw 3
  proof3_1=$(dune exec deku-p/src/core/tezos_interop/tests/proof_test.exe $op_hash3)

  message Proof of withdraw 2
  proof2_1=$(dune exec deku-p/src/core/tezos_interop/tests/proof_test.exe $op_hash2)

  message Proof of withdraw 1, again
  proof1_2=$(dune exec deku-p/src/core/tezos_interop/tests/proof_test.exe $op_hash1)

  sleep 30

  if [ "$proof3_1" ]
  then
    echo "Withdraw 3 did NOT fail"
    test-failed
  fi

  if [ -z "$proof1_1" ] || [ -z "$proof1_2" ] || [ -z "$proof2_1" ]
  then
    echo "One of the correct withdraws failed"
    echo "$proof1_1"
    echo "$proof1_2"
    echo "$proof2_1"
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
}

case "$1" in
public)
  public-setup
  run-test
  ;;
local)
  local-setup
  run-test
  killall deku-node
  ;;
*)
  local-setup
  run-test
  killall deku-node
  ;;
esac
