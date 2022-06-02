#! /usr/bin/env bash
set -e 

# Profiling deku node
#
# - `-F 99`: Sampling CPU stacks at 99 Hertz*
# - `-g`: enable call-graph (stack chain/backtrace) recording for both kernel space 
#        and user space
# - `--call-graph dwarf`: set up and enable call-graph mode, get backtraces by using `dwarf` option
# - `-a`: samples across all CPUs
#
# **Note:**
# The rate for perf is 99 Hertz, the generated FlameGraph from a 1000 Hertz profile.
# Choosing 99 Hertz instead of 100 Hertz, is to avoid accidentally sampling in 
# lockstep with some periodic activity, which would produce skewed results.
# You can also increase to higher rates (eg, up to 997 Hertz) for finer resolution.
# Bear in mind that higher frequencies means higher overhead

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
      | dot -Tpng -o ~/deku/benchmarks/profiling/perf-deku-callgraph.svg
}

# Memory profiling using valgrind
# - Massif:
#   + Heap blocks;
#   + Heap administration blocks;
#   + Stack sizes. 
# 
# --smc-check=all: tells Valgrind that the program may use self-modifying code
# --massif-out-file=./massif.out.%p: define the name output by massif
# --detailed-freq=1000000 tells Massif to do detailed snapshots
#   (which are more informative but more costly) only every 1,000,000th snapshot,
#   which is less often than the default of every 10th snapshot.  
#   This makes it run a bit faster. This is fine because Massif always 
#   takes a detailed snapshot at the peak memory consumption point.
# --leak-check=full: turns on the detailed memory leak detector. When using this
#   the program will run much slower (eg. 20 to 30 times) than normal, and use a lot 
# --log-file=valgrind-out.txt: write to a file. Useful when output exceeds terminal space.
#   of memory. Memcheck will issue messages about memory errors and leaks that it detects.
# --trace-children=yes: valgrind will trace into sub-processes initated via the `exec`
#   system call. This is necessary for multi-process programs.
# -v: be more verbose. Gives extra information on various aspects of the program, such as:
#   the shared objects loaded, the suppressions used, the progress of the instrumentation
#   and execution engines.
#
# - Generate a call tree:
#  --xtree-memory=full: which can then be inspected using kcachegrind. 
#   The call tree filename is xtmemory.kcg.[pid] 
#
# - Investigate memory while running:(?)
#   --vgdb=yes 
#   --vgdb-error=0:
#   To investigate the memory usage or call tree while an executable is running,
#   valgrind needs to be used through a gdb server. 

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
