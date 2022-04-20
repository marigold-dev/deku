(*
  - build with instrument landmarks
  esy x dune build --instrument-with landmarks ./bench_deku.exe
  - build with profiling:
   esy x dune exec --context profiling ~/deku/_build/profiling/benchmarks/landmarks/bench_deku.exe
  - build with profiling-auto:
  esy x dune exec --context profiling-auto ~/deku/_build/profiling-auto/benchmarks/landmarks/bench_deku.exe
*)
let main = Bench_ledger.benchmark_ledger ()
