

set -e


setup(){
# add deku tezos secret key in the tezos chain as bob

export DEKU_TEZOS_SECRET=edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq;

# add account for bob to have tez

nix run github:marigold-dev/tezos-nix#tezos-client -- import secret key bob unencrypted:edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq --force;

# setup, deploy and start the deku-chain
dune exec -- _build/default/sandbox/deku_sandbox.exe setup;

dune exec -- _build/default/sandbox/deku_sandbox.exe deploy-dummy-ticket;

dune exec -- _build/default/sandbox/deku_sandbox.exe start
}

load_test(){
    # load-test-tps
dune exec -- _build/default/sandbox/deku_sandbox.exe load-test-tps >& ~/deku/tests/e2e/bench_tps/load_test_tps.csv
}

load_test_offline(){
    #load-test-tps-offline
dune exec -- _build/default/sandbox/deku_sandbox.exe load-test-tps-offline >& ~/deku/tests/e2e/bench_tps/load_test_tps_offline.csv
}

network(){
    # stressing the network with no-op transaction with verify signatures
dune exec -- _build/default/sandbox/deku_sandbox.exe network-msg >& ~/deku/tests/e2e/bench_tps/network_msg.csv
}

bench-hash(){
    # core-bench state hash
  dune exec -- _build/default/benchmarks/state_hash/bench_state_hash.exe state-hash >& ~/deku/benchmarks/state_hash/bench_state_hash.csv
}

case "$1" in
setup)
  setup
  ;;
load-test)
  load_test
  ;;
load-test-offline)
  load_test_offline
  ;;
network)
  network
  ;;
bench_hash)
  bench-hash
  ;;
esac
