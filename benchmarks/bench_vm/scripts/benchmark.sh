# Required programs: 
#   csvcut (https://github.com/wireservice/csvkit)
#   graph-cli (https://github.com/mcastorina/graph-cli)

# build benchmarks
esy b dune build @benchmarks/bench &> /dev/null

# TODO: figure out how to substitute this in the for loop for more modularity
#BENCHMARKS = "expr" "gas" "prim" "recursive"

touch ./benchmarks/bench_vm/results/bench.txt

# "gas" "prim" "recursive"

for benchmark in "trivial"
  do
    ./_esy/default/build/default/benchmarks/bench_vm/benchmarks.exe $benchmark -sexp | \
    ./benchmarks/bench_vm/scripts/clean.sh | \
    tee ./benchmarks/bench_vm/results/$benchmark.csv >> $bench.csv 
  done