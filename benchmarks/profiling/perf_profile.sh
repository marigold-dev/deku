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

# Main
help(){
  echo "$0 automates profiling a Tezos testnet node and setup of a Deku cluster."
  echo ""
  echo "Usage: $0 run|flame-graph|call-graph"
  echo "Commands:"
  echo "run"
  echo " Profiling deku node"
  echo "flamegraph"
  echo " Read perf.data using FlameGraph, need to have FlameGraph downloaded\
         and the perf.data"
  echo "callgraph"
  echo " Read perf.data using gprof2dot, need to have the perf.data"
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
*)
  help 
  ;;
esac
