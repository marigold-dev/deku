(*
   Build: esy x dune build
   Run profile perf:
   sudo perf record -a -F 99 -g ~/deku/_build/default/benchmarks/profiling_perf/profile_deku.exe
   sudo perf script -i perf.data > profile_deku.linux-perf.txt -f

  Note: each run esy x dune build, remove the perf.data and old perf.data  

  sudo perf report -n

  For Flamegraph need to download the Flamegraph from git:

  # git clone https://github.com/brendangregg/FlameGraph 
  # cd FlameGraph

  # perf record --callgraph dwarf -- program-to-run program-arguments

  $ sudo perf record --call-graph dwarf -- ~/deku/_build/default/benchmarks/profiling_perf/profile_deku.exe
  # sudo perf script | ./stackcollapse-perf.pl | ./flamegraph.pl > perf-deku-flamegraph.svg

  # profile for sandbox deku run
  sudo perf record -F 99 --call-graph dwarf -- ./sandbox.sh start

  --using hospot to read perf.data
  https://github.com/KDAB/hotspot
  hotspot /path/to/perf.data


  -- Memory profiling the C heap
  - install `valgrind` and `massif-visualizer`

  Then run:
  `valgrind --tool=massif ~/deku/_build/default/benchmarks/profiling_perf/profile_deku.exe`

  where:
  --tool=<name>: use the Valgrind tool named <name> [memcheck]

  - Using Ctrl-C then seeing the result with:
  `massif-visualizer massif.out.pid`

  in this example it is: 
  `massif-visualizer massif.out.45147`

-- find memory leak 
https://blog.janestreet.com/finding-memory-leaks-with-memtrace/
spacetime is another profiling memory, it would gather data on all the allocations performed
by a program. It was very useful for finding memory leaks but had a high overhead and required using
a special configuration of the OCaml compiler. Its profiles could become very large and require
huge amounts of memory to process.

The memtrace support:
+ profiles a sample of the allocations, not all of them
+ works with the ordinary OCaml compiler
+ it is supported by all platforms
+ it can run with low enough overhead to be used in production systems

install: opam switch create memtrace
eval $(opam env --switch=memtrace)

--another lib for memory leak
https://github.com/jhjourdan/statmemprof-emacs/
http://doc.tzalpha.net/developer/profiling.html




*)

let main () =
  let _ = Memtrace.trace_if_requested ~context:"Profile_deku with memtrace" () in
  Profile_ledger.profile_ledger ();
  Profile_tezos_interop.profile_tezos_interop ();
  ()
