DEKU_TEZOS_RPC_NODE="http://localhost:20000"
SECRET_KEY="edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq"
storage_path=${1:-./networks/flextesa}
dir=$(dirname $0)

tezos_client(){
  docker exec -t deku_flextesa tezos-client "$@"
}


message() {
  echo -e "\e[35m\e[1m**************************    $*    ********************************\e[0m"
}

tezos_client --endpoint "$DEKU_TEZOS_RPC_NODE" import secret key myWallet "unencrypted:$SECRET_KEY" --force


deploy_contract() {
    message "Deploying new $1 contract"

    wallet=${4:-myWallet}
    # Compiles an initial storage for a given contract to a Michelson expression.
    # The resulting Michelson expression can be passed as an argument in a transaction which originates a contract.
    storage=$(ligo compile storage "$2" "$3")

    # Compiles a contract to Michelson code.
    # Expects a source file and an entrypoint function.
    contract=$(ligo compile contract "$2")

    echo "Originating $1 contract"
    sleep 2
    tezos_client \
        --wait 1 \
        --endpoint "$DEKU_TEZOS_RPC_NODE" originate contract "$1" \
        transferring 0 from $wallet \
        running "$contract" \
        --init "$storage" \
        --burn-cap 2 \
        --force
}

deploy_contract "consensus" \
  "./deku-p/src/core/tezos_interop/consensus.mligo" \
  "$(cat "$storage_path/consensus_storage.mligo")"

deploy_contract "dummy_ticket" \
  "./dummy_ticket.mligo" \
  "()"