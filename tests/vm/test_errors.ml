open Lambda_vm

module Testable = Vm_test.Testable

let sender =
  "tz1ibMpWS6n6MJn73nQHtK5f4ogyYC1z9T9z" |> Core.Address.of_string |> Option.get

let check_execution_error ~actual ~expected =
  let open Vm_test in
  match actual with
  | Error (Execution_error error) ->
    Alcotest.(check Testable.execution_error) "Same error" expected error
  | Ok _ -> Alcotest.fail "Ast shouldn't execute"
  | Error (Compilation_error error) ->
    Alcotest.failf "%a" Compiler.pp_error error

let check_compilation_error ~actual ~expected =
  let open Vm_test in
  match actual with
  | Error (Compilation_error error) ->
    Alcotest.(check Testable.compilation_error) "Same error" expected error
  | Ok _ -> Alcotest.fail "Ast shouldn't execute"
  | Error (Execution_error error) ->
    Alcotest.failf "%a" Interpreter.pp_error error

let test_compilation_undefined_variable () =
  let script = [%lambda_vm.script fun _ -> x] in
  check_compilation_error
    ~actual:(Vm_test.execute_ast sender 2000 (Int64 0L) script)
    ~expected:(Compiler_error Compiler.Undefined_variable)

let test_fst_value_is_not_pair () =
  let script = [%lambda_vm.script fun _ -> (fst 1L, (0L, 0L))] in
  check_execution_error
    ~actual:(Vm_test.execute_ast sender 1201 (Int64 0L) script)
    ~expected:(Interpreter_error Interpreter.Value_is_not_pair)

let test_snd_value_is_not_pair () =
  let script = [%lambda_vm.script fun _ -> (snd 1L, (0L, 0L))] in
  check_execution_error
    ~actual:(Vm_test.execute_ast sender 1201 (Int64 0L) script)
    ~expected:(Interpreter_error Interpreter.Value_is_not_pair)

let test_neg_value_is_not_int64 () =
  let script = [%lambda_vm.script fun _ -> (not (0L, 0L), (0L, 0L))] in
  check_execution_error
    ~actual:(Vm_test.execute_ast sender 1601 (Int64 0L) script)
    ~expected:(Interpreter_error Interpreter.Value_is_not_int64)

let test_op2_value_is_not_int64 () =
  let script = [%lambda_vm.script fun _ -> ((0L, 0L) + 0L, (0L, 0L))] in
  check_execution_error
    ~actual:(Vm_test.execute_ast sender 2001 (Int64 0L) script)
    ~expected:(Interpreter_error Interpreter.Value_is_not_int64)

let test_if_value_is_not_int64 () =
  let script =
    [%lambda_vm.script fun _ -> if (0L, 0L) then 1L else (1L, (0L, 0L))] in
  check_execution_error
    ~actual:(Vm_test.execute_ast sender 1601 (Int64 0L) script)
    ~expected:(Interpreter_error Interpreter.Value_is_not_int64)

let test_value_is_not_function () =
  let script = [%lambda_vm.script fun _ -> (0L 0L, (0L, 0L))] in
  check_execution_error
    ~actual:(Vm_test.execute_ast sender 1201 (Int64 0L) script)
    ~expected:(Interpreter_error Interpreter.Value_is_not_function)

let test_pattern1_value_is_not_pair () =
  let script = [%lambda_vm.script fun _ -> (0L, 0L)] in
  check_execution_error
    ~actual:(Vm_test.execute_ast sender 701 (Int64 0L) script)
    ~expected:(Interpreter_error Interpreter.Value_is_not_pair)

let test_pattern2_value_is_not_pair () =
  let script = [%lambda_vm.script fun _ -> 0L] in
  check_execution_error
    ~actual:(Vm_test.execute_ast sender 301 (Int64 0L) script)
    ~expected:(Interpreter_error Interpreter.Value_is_not_pair)

let test_pattern3_value_is_not_zero () =
  let script = [%lambda_vm.script fun _ -> (0L, (1L, 0L))] in
  check_execution_error
    ~actual:(Vm_test.execute_ast sender 1101 (Int64 0L) script)
    ~expected:(Interpreter_error Interpreter.Value_is_not_zero)

let test_pattern4_value_is_not_zero () =
  let script = [%lambda_vm.script fun _ -> (0L, (0L, 1L))] in
  check_execution_error
    ~actual:(Vm_test.execute_ast sender 1101 (Int64 0L) script)
    ~expected:(Interpreter_error Interpreter.Value_is_not_zero)

let test_compilation =
  let open Alcotest in
  ( "Compilation and execution errors",
    [
      test_case "Compilation - Undefined variable" `Quick
        test_compilation_undefined_variable;
      test_case "Fst - Value should be pair" `Quick test_fst_value_is_not_pair;
      test_case "Snd - Value should be pair" `Quick test_snd_value_is_not_pair;
      test_case "Neg - Value should be int64" `Quick test_neg_value_is_not_int64;
      test_case "Op2 - Value should be int64" `Quick test_op2_value_is_not_int64;
      test_case "If - Value should be int64" `Quick test_if_value_is_not_int64;
      test_case "Value is not a function" `Quick test_value_is_not_function;
    ] )

let test_execution =
  let open Alcotest in
  ( "Execution pattern errors",
    [
      test_case "Pattern 1 - Value should be pair" `Quick
        test_pattern1_value_is_not_pair;
      test_case "Pattern 2 - Value should be pair" `Quick
        test_pattern2_value_is_not_pair;
      test_case "Pattern 3 - Value should be zero" `Quick
        test_pattern3_value_is_not_zero;
      test_case "Pattern 4 - Value should be zero" `Quick
        test_pattern4_value_is_not_zero;
    ] )
