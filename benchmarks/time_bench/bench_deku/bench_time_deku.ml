(*
  TODO: write script
  esy build:
  - esy x dune build
  run:
  - esy x dune exec 
    ~/deku/_build/default/benchmarks/time_bench/bench_deku/bench_time_deku.exe
*)

let main = Bench_time_ledger.benchmark_ledger ()
