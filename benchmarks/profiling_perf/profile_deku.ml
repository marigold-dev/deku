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
*)
let main () =
  Profile_ledger.profile_ledger ();
  Profile_tezos_interop.profile_tezos_interop ()
