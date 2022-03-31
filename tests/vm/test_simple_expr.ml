open Lambda_vm
module Testable = Vm_test.Testable

let test_increment () =
  let script = [%lambda_vm.script fun x -> (x + 1L, (0L, 0L))] in
  let result = Vm_test.execute_ast_exn 1901 (Int64 42L) script in
  let expected_value =
    Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 43L) in
  Alcotest.(check Testable.value) "Same value" expected_value result.storage

let test_decrement () =
  let script = [%lambda_vm.script fun x -> (x - 1L, (0L, 0L))] in
  let result = Vm_test.execute_ast_exn 1901 (Int64 42L) script in
  let expected_value =
    Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 41L) in
  Alcotest.(check Testable.value) "Same value" expected_value result.storage

let test_add_pair () =
  let script = [%lambda_vm.script fun pair -> (fst pair + snd pair, (0L, 0L))] in

  let result =
    Vm_test.execute_ast_exn 2901 (Pair (Int64 23L, Int64 28L)) script in
  let expected_value =
    Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 51L) in

  Alcotest.(check Testable.value) "Same value" expected_value result.storage

let test_if_expr () =
  let script =
    [%lambda_vm.script
      fun param ->
        ((if fst param then snd param + 1L else snd param - 1L), (0L, 0L))]
  in

  (* Check increment *)
  let result =
    Vm_test.execute_ast_exn 4001 (Pair (Int64 1L, Int64 51L)) script in
  let expected_value =
    Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 52L) in
  Alcotest.(check Testable.value) "Same value" expected_value result.storage;

  (* Check decrement *)
  let result =
    Vm_test.execute_ast_exn 4001 (Pair (Int64 0L, Int64 33L)) script in
  let expected_value =
    Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 32L) in
  Alcotest.(check Testable.value) "Same value" expected_value result.storage

let test_lambda () =
  let script =
    let increment_lambda = [%lambda_vm fun x -> x + 1L] in
    let decrement_lambda = [%lambda_vm fun x -> x - 1L] in
    [%lambda_vm.script
      fun param ->
        ( (fun inc ->
            if inc then [%e increment_lambda] else [%e decrement_lambda])
            (fst param) (snd param),
          (0L, 0L) )] in
  (* Check increment *)
  let result =
    Vm_test.execute_ast_exn 6501 (Pair (Int64 1L, Int64 99L)) script in
  let expected_value =
    Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 100L) in

  Alcotest.(check Testable.value) "Same value" expected_value result.storage;

  (* Check decrement *)
  let result =
    Vm_test.execute_ast_exn 6501 (Pair (Int64 0L, Int64 33L)) script in
  let expected_value =
    Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 32L) in

  Alcotest.(check Testable.value) "Same value" expected_value result.storage

let test_lambda_application () =
  let script = [%lambda_vm.script fun y -> (fun x -> (x, (0L, 0L))) y] in
  let result = Vm_test.execute_ast_exn 2000 (Int64 45L) script in
  let expected_value =
    Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 45L) in

  Alcotest.(check Testable.value) "Same value" expected_value result.storage

let test =
  let open Alcotest in
  ( "Simple with simple expressions",
    [
      test_case "Increment" `Quick test_increment;
      test_case "Decrement" `Quick test_decrement;
      test_case "Add pair" `Quick test_add_pair;
      test_case "If expr" `Quick test_if_expr;
      test_case "Lambda" `Quick test_lambda;
      test_case "Lambda application" `Quick test_lambda_application;
    ] )
