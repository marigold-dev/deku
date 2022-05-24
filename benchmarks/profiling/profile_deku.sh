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
# Massif:
# - Heap blocks;
# - Heap administration blocks;
# - Stack sizes. 
# 
# --smc-check=all: tells Valgrind that the program may use self-modifying code
# --massif-out-file=./massif.out.%p 
# --detailed-freq=1000000 tells Massif to do detailed snapshots
# (which are more informative but more costly) only every 1,000,000th snapshot,
# which is less often than the default of every 10th snapshot.  
# This makes it run a bit faster.  This is fine because Massif always 
# takes a detailed snapshot at the peak memory consumption point
#
# Generate a call tree:
#  --xtree-memory=full:  which can then be inspected using kcachegrind. 
# The call tree filename is xtmemory.kcg.[pid] 
#
# Install kachegrind:
# sudo apt update
# sudo apt-get install kcachegrind
#
# Investigate memory while running:(?)
# --vgdb=yes 
# --vgdb-error=0:
# To investigate the memory usage or call tree while an executable is running,
# valgrind needs to be used through a gdb server. 

memory_profile(){
   cd .. && cd .. && ./sandbox.sh tear-down \
         && ./sandbox.sh setup \
         && valgrind \
             --smc-check=all \
             --detailed-freq=1000000 \
             --trace-children=yes \
             --xtree-memory=full \
             -v -d --trace-signals=yes\
             --tool=massif \
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
