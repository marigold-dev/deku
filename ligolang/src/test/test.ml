(* -*- compile-command: "cd .. ; dune runtest" -*- *)

open Test_helpers

let () =
  Printexc.record_backtrace true ;
    run_test @@ test_suite "LIGO"
  [
    Integration_tests.main ;
    Spilling_tests.main ;
  ];()
