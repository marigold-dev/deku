open Test_helpers

let () =
  Printexc.record_backtrace true ;
  run_test @@ test_suite "LIGO" [
    Md_file_tests.main ;
  ] ;
  ()
