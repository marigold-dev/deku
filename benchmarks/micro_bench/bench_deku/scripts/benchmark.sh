# build benchmarks
esy b dune build @benchmarks/bench &> /dev/null

RESULTPATH=./benchmarks/micro_bench/bench_deku/results

# TODO: figure out how to substitute this in the for loop for more modularity
#BENCHMARKS = "expr" "gas" "prim" "recursive"

for benchmark in "interop" "ledger" "patricia" "rpc" "validators"
  do
    ./_esy/default/build/default/benchmarks/micro_bench/bench_deku/bench_deku.exe $benchmark > \
    $RESULTPATH/$benchmark.txt
  done
