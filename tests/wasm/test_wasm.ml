let () =
  let open Alcotest in
  run "Wasm" [Test_parsing.test; Test_runtime.test]
