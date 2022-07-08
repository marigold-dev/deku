set -e

# add deku tezos secret key in the tezos chain as bob

export DEKU_TEZOS_SECRET=edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq

# add account for bob to have tez

nix run github:marigold-dev/tezos-nix#tezos-client -- import secret key bob unencrypted:edsk3QoqBuvdamxouPhin7swCvkQNgq4jP5KZPbwWNnwdZpSpJiEbq --force


dune exec -- _build/default/sandbox/deku_sandbox.exe setup

dune exec -- _build/default/sandbox/deku_sandbox.exe start

dune exec -- _build/default/sandbox/deku_sandbox.exe deploy-dummy-ticket

# Remember to deploy dummy ticket before using these tests below
dune exec -- _build/default/sandbox/deku_sandbox.exe load-test-tps >& ~/deku/tests/e2e/bench_tps/load_test_tps.csv

dune exec -- _build/default/sandbox/deku_sandbox.exe load-test-tps-offline >& ~/deku/tests/e2e/bench_tps/load_test_tps_offline.csv

dune exec -- _build/default/sandbox/deku_sandbox.exe network-msg >& ~/deku/tests/e2e/bench_tps/network_msg.csv

# benchmark for state hash core bench
dune exec -- _build/default/benchmarks/state_hash/bench_state_hash.exe state-hash >& ~/deku/benchmarks/state_hash/bench_state_hash.csv
