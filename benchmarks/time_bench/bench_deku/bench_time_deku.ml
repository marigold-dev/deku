(*
  TODO: write script
  esy build:
  - esy x dune build
  run:
  - esy x dune exec 
    ~/deku/_build/default/benchmarks/time_bench/bench_deku/bench_time_deku.exe
*)

let main =
  Bench_time_ledger.benchmark_ledger ();
  Bench_time_patricia.benchmark_patricia ();
  Bench_time_tezos_interop.benchmark_tezos_interop ()
