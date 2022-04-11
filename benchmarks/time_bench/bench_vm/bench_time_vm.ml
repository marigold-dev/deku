open Bench_time_prim

(*
  TODO: write script
  esy build:
  - esy x dune build
  run:
  - esy x dune exec 
    ~/deku/_build/default/benchmarks/ocaml_benchmarks/lambda_vm/bench_prim.exe
*)

let main =
  bench_compile_value_neg
    "Compile value negative primitive with n = 0, 1, 2; init_gas = 200" ();
  bench_compile_neg "Compile negative primitive script, init_gas = 1501" ();
  bench_execute_neg "Execute negative primitive with n = 0, 1, 2" ()
