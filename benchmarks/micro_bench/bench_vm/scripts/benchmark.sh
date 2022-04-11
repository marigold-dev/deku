# build benchmarks
esy b dune build @benchmarks/bench &> /dev/null

RESULTPATH=./benchmarks/micro_bench/bench_vm/results

# TODO: figure out how to substitute this in the for loop for more modularity
#BENCHMARKS = "expr" "gas" "prim" "recursive"

for benchmark in "simple-expr" "gas" "prim" "recursive"
  do
    ./_esy/default/build/default/benchmarks/micro_bench/bench_vm/benchmarks.exe $benchmark > \
    $RESULTPATH/$benchmark.txt
  done
