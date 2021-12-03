
open Test_helpers

let () = 
  Printexc.record_backtrace true ;
    run_test @@ test_suite "Type inference"
  [
    Heuristic_tc_fundep_tests.main ;
    Heuristic_break_ctor_tests.main ;
    Heuristic_specialize1_tests.main ;
    Heuristic_access_label_tests.main ;
    Typechecker_tests.main ;
    Db_index_tests.main ;
    Typer_tests.main ;
  ];
  ()
