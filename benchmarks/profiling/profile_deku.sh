#! /usr/bin/env bash
set -e 

# Profiling deku node
run(){
cd .. && cd .. \
      && ./sandbox.sh tear-down \
      && ./sandbox.sh setup \
      && perf record -F 99 -a -g --call-graph dwarf ./sandbox.sh start \
      && sleep 10
}

# Read perf.data using FlameGraph
flame_graph(){
    cp ~/deku/perf.data ~/FlameGraph/perf.data \
      && cd ~/FlameGraph && perf script \
          | ./stackcollapse-perf.pl \
          | ./flamegraph.pl > ~/deku/benchmarks/profiling/perf-deku-flamegraph.svg  
}

# Read perf.data using gprof2dot
call_graph(){
      cd .. && cd .. && perf script \
      | c++filt | gprof2dot -f perf \
      | dot -Tpng -o ~/deku/benchmarks/profiling/perf-deku-callgraph.png
}

# Memory profiling using valgrind
memory_profile(){
   cd .. && cd .. && ./sandbox.sh tear-down \
         && ./sandbox.sh setup \
         && time valgrind --tool=massif \
             --xtree-memory=full \
             --verbose \
             --massif-out-file=~/deku/benchmarks/profiling/massif.out.%p \
            ./sandbox.sh start
}


# Main
help(){
  echo "$0 automates profiling a Tezos testnet node and setup of a Deku cluster."
  echo ""
  echo "Usage: $0 run|flame-graph|call-graph|memory-profile"
  echo "Commands:"
  echo "run"
  echo " Profiling deku node"
  echo "flamegraph"
  echo " Read perf.data using FlameGraph, need to have FlameGraph downloaded\
         and the perf.data"
  echo "callgraph"
  echo " Read perf.data using gprof2dot, need to have the perf.data"
  echo "memory-profile"
  echo " Memory profiling the C heap of deku node using valgrind"
}

case "$1" in 
run)
  run
  ;;
flame-graph)
  flame_graph
  ;;
call-graph)
  call_graph
  ;;
memory-profile)
  memory_profile
  ;;
*)
  help 
  ;;
esac
