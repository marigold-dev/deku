let () =
  let open Alcotest in
  run
    "Lambda VM"
    [
      Test_errors.test_compilation;
      Test_errors.test_execution;
      Test_prim.test;
      Test_recursion.test;
      Test_simple_expr.test;
      Test_gas.test;
      Test_io.test;
      Test_voting_contract.test;
    ]
