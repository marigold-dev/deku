open Deku_tests
open Ledger_tests

(*
  - build with instrument and enviroment:
      esy x dune build --instrument-with landmarks ./bench_ledger.exe

  - run with profiling:
    esy x dune exec --context profiling 
      ~/deku/_build/profiling/benchmarks/landmarks/bench_ledger.exe

  - run with profiling-auto:
    esy x dune exec --context profiling-auto 
       ~/deku/_build/profiling-auto/benchmarks/landmarks/bench_ledger.exe
*)

let main = Landmark.register "main"

let call_ticket = Landmark.register "make_ticket"

let call_address = Landmark.register "make_address"

let call_make_ticket = Landmark.wrap call_ticket (fun () -> make_ticket ()) ()

let call_make_address =
  Landmark.wrap call_address (fun () -> make_address ()) ()

let benchmark_ledger () =
  let open Landmark in
  (* uncomment this function when one wants to use the --context profiling/profiling-auto *)
  start_profiling ();
  enter main;
  ignore call_make_ticket;
  ignore call_make_address;
  exit main
