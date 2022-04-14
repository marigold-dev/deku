let () =
  let open Alcotest in
  run "Contracts_lambda"
    [Test_origination.test_origination; Test_invocation.test_invocation]
