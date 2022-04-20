open Deku_tests
open Ledger_tests

(*
  - build with instrument and enviroment:
      esy x dune build --instrument-with landmarks ./bench_ledger.exe

  - build with profiling:
    esy x dune exec --context profiling 
      ~/deku/_build/profiling/benchmarks/landmarks/bench_ledger.exe

  - build with profiling-auto:
    esy x dune exec --context profiling-auto 
       ~/deku/_build/profiling-auto/benchmarks/landmarks/bench_ledger.exe
*)

let main = Landmark.register "main"

let call_ticket = Landmark.register "make_ticket"

let call_ledger_make_ticket =
  Landmark.wrap call_ticket (fun () -> make_ticket ()) ()

let benchmark_ledger () =
  let open Landmark in
  enter main;
  ignore call_ledger_make_ticket;
  exit main
