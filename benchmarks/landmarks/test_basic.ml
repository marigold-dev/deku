(* - build with ocaml_benchmark instrument:
      dune build --instrument-with landmarks ./test_basic.exe
      OCAML_LANDMARKS=on ~/deku/_build/default/benchmarks/landmarks/test_basic.exe

   - build with profiling-auto
     dune exec --context profiling-auto ~/deku/_build/profiling-auto/benchmarks/landmarks/test_basic.exe
   - build with profiling
     dune exec --context profiling ~/deku/_build/profiling/benchmarks/landmarks/test_basic.exe
*)

let[@landmark] zzz () = Unix.sleep 1

let f () =
  zzz ();
  zzz ()

let main () =
  zzz ();
  f ();
  f ()

let () = main ()
