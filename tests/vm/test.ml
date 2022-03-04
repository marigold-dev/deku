open Lambda_vm

let compilation_error_to_string: compile_error -> string = function
  | Undefined_variable -> "Undefined_variable"

let execution_error_to_string = function
  | Undefined_variable -> "Undefined_variable"
  | Over_applied_primitives -> "Over_applied_primitives"
  | Value_is_not_pair -> "Value_is_not_pair"
  | Value_is_not_int64 -> "Value_is_not_int64"
  | Value_is_not_function -> "Value_is_not_function"
  | Value_is_not_zero -> "Value_is_not_zero"

let compile_exn gas script =
  match compile gas script with
  | Ok value -> value
  | Error error -> failwith (compilation_error_to_string error)

let compile_value_exn gas value =
  match compile_value gas value with
  | Ok value -> value
  | Error error -> failwith (compilation_error_to_string error)

let execute_exn gas arg script =
  match execute gas ~arg script with
  | Ok value -> value
  | Error error -> failwith (execution_error_to_string error)

let execute_ast gas arg script =
  (* TODO: Use different gas to different stuff *)
  let gas = Gas.make ~initial_gas:gas in
  let script = compile_exn gas script in
  let arg = compile_value_exn gas arg in
  execute_exn gas arg script

module Testable = struct
  let value = Alcotest.testable Lambda_vm.pp_value ( = )
end

let test_increment () =
  let script =
    Ast.{ param = "x"
        ; code = Pair { first = App { funct = Prim Add
                                    ; arg = Pair { first = Var "x"
                                                 ; second = Const 1L } }
                      ; second = Pair { first = Const 0L
                                      ; second = Const 0L } } }
  in
  let result = execute_ast 1901 (Int64 42L) script in
  let expected_value = compile_value_exn (Gas.make ~initial_gas:101) (Int64 43L) in

  Alcotest.(check Testable.value) "Same value" expected_value result.storage

let test_decrement () =
  let script =
    Ast.{ param = "x"
        ; code = Pair { first = App { funct = Prim Sub
                                    ; arg = Pair { first = Var "x"
                                                 ; second = Const 1L } }
                      ; second = Pair { first = Const 0L
                                      ; second = Const 0L } } }
  in

  let result = execute_ast 1901 (Int64 42L) script in
  let expected_value = compile_value_exn (Gas.make ~initial_gas:101) (Int64 41L) in

  Alcotest.(check Testable.value) "Same value" expected_value result.storage

let () =
  let open Alcotest in
  run "Lambda VM" [
      "Simple with primitive operations",
      [ test_case "Increment" `Quick test_increment
      ; test_case "Decrement" `Quick test_decrement ] ]
