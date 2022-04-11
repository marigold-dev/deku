(*
  TODO: write script
  esy build:
  - esy x dune build
  run:
  - esy x dune exec 
    ~/deku/_build/default/benchmarks/time_bench/bench_vm/bench_time_vm.exe
*)

let main =
  Bench_time_prim.benchmark_prim ();
  Bench_time_recursion.benchmark_recursion ()
