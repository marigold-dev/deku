let () =
  let open Alcotest in
  run "Contracts_lambda" [Test_origination.test_origination]
