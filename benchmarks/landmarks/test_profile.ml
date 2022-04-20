(*
  - build with instrument and enviroment:
      dune build --instrument-with landmarks ./test_profile.exe
      OCAML_LANDMARKS=on ~/deku/_build/default/benchmarks/landmarks/test_profile.exe

  - build with profiling:
    dune exec --context profiling 
      ~/deku/_build/profiling/benchmarks/landmarks/test_profile.exe

  - build with profiling-auto:
    dune exec --context profiling-auto 
       ~/deku/_build/profiling-auto/benchmarks/landmarks/test_profile.exe
*)

let call = Landmark.register "fib"
let main = Landmark.register "main"

let rec fib n =
  Landmark.wrap call
    (fun n -> if n <= 1 then 1 else fib (n - 1) + fib (n - 2))
    n

let () =
  let open Landmark in
  enter main;
  Printf.printf "Fib 7: %d\n%!" (fib 7);
  exit main
