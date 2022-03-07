open Lambda_vm

module Vm_test = struct
  type error =
    | Compilation_error of compile_error
    | Execution_error   of execution_error

  let compilation_error_to_string : compile_error -> string = function
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

  let execute_ast_exn gas arg script =
    (* TODO: Use different gas to different stuff *)
    let gas = Gas.make ~initial_gas:gas in
    let script = compile_exn gas script in
    let arg = compile_value_exn gas arg in
    execute_exn gas arg script

  let execute_ast gas arg script =
    let gas = Gas.make ~initial_gas:gas in
    match compile_value gas arg with
    | Ok arg -> (
      match compile gas script with
      | Ok ir -> (
        match execute gas ~arg ir with
        | Ok result -> Ok result
        | Error error -> Error (Execution_error error))
      | Error error -> Error (Compilation_error error))
    | Error error -> Error (Compilation_error error)
end

module Testable = struct
  let value = Alcotest.of_pp Lambda_vm.pp_value

  let execution_error =
    Alcotest.of_pp (fun fmt error ->
        Format.fprintf fmt "%s" (Vm_test.execution_error_to_string error))

  let compilation_error =
    Alcotest.of_pp (fun fmt error ->
        Format.fprintf fmt "%s" (Vm_test.compilation_error_to_string error))
end

let test_increment () =
  let script =
    Ast.
      {
        param = "x";
        code =
          Pair
            {
              first =
                App
                  {
                    funct = App { funct = Prim Add; arg = Var "x" };
                    arg = Const 1L;
                  };
              second = Pair { first = Const 0L; second = Const 0L };
            };
      } in
  let result = Vm_test.execute_ast_exn 1901 (Int64 42L) script in
  let expected_value =
    Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 43L) in
  Alcotest.(check Testable.value) "Same value" expected_value result.storage

let test_decrement () =
  let script =
    Ast.
      {
        param = "x";
        code =
          Pair
            {
              first =
                App
                  {
                    funct = App { funct = Prim Sub; arg = Var "x" };
                    arg = Const 1L;
                  };
              second = Pair { first = Const 0L; second = Const 0L };
            };
      } in

  let result = Vm_test.execute_ast_exn 1901 (Int64 42L) script in
  let expected_value =
    Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 41L) in
  Alcotest.(check Testable.value) "Same value" expected_value result.storage

let test_add_pair () =
  let script =
    Ast.
      {
        param = "pair";
        code =
          Pair
            {
              first =
                App
                  {
                    funct = App { funct = Prim Add; arg = Fst (Var "pair") };
                    arg = Snd (Var "pair");
                  };
              second = Pair { first = Const 0L; second = Const 0L };
            };
      } in

  let result =
    Vm_test.execute_ast_exn 2501 (Pair (Int64 23L, Int64 28L)) script in
  let expected_value =
    Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 51L) in

  Alcotest.(check Testable.value) "Same value" expected_value result.storage

let test_if_expr () =
  let script =
    Ast.
      {
        param = "param";
        code =
          Pair
            {
              first =
                If
                  {
                    predicate = Fst (Var "param");
                    consequent =
                      App
                        {
                          funct =
                            App { funct = Prim Add; arg = Snd (Var "param") };
                          arg = Const 1L;
                        };
                    alternative =
                      App
                        {
                          funct =
                            App { funct = Prim Sub; arg = Snd (Var "param") };
                          arg = Const 1L;
                        };
                  };
              second = Pair { first = Const 0L; second = Const 0L };
            };
      } in

  (* Check increment *)
  let result =
    Vm_test.execute_ast_exn 3501 (Pair (Int64 1L, Int64 51L)) script in
  let expected_value =
    Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 52L) in
  Alcotest.(check Testable.value) "Same value" expected_value result.storage;

  (* Check decrement *)
  let result =
    Vm_test.execute_ast_exn 3501 (Pair (Int64 0L, Int64 33L)) script in
  let expected_value =
    Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 32L) in
  Alcotest.(check Testable.value) "Same value" expected_value result.storage

let test_lambda () =
  let script =
    let increment_lambda =
      Ast.(
        Lam
          ( "x",
            App
              {
                funct = App { funct = Prim Add; arg = Var "x" };
                arg = Const 1L;
              } )) in
    let decrement_lambda =
      Ast.(
        Lam
          ( "x",
            App
              {
                funct = App { funct = Prim Sub; arg = Var "x" };
                arg = Const 1L;
              } )) in
    Ast.
      {
        param = "param";
        code =
          Pair
            {
              first =
                App
                  {
                    funct =
                      App
                        {
                          funct =
                            Lam
                              ( "inc",
                                If
                                  {
                                    predicate = Var "inc";
                                    consequent = increment_lambda;
                                    alternative = decrement_lambda;
                                  } );
                          arg = Fst (Var "param");
                        };
                    arg = Snd (Var "param");
                  };
              second = Pair { first = Const 0L; second = Const 0L };
            };
      } in

  (* Check increment *)
  let result =
    Vm_test.execute_ast_exn 6101 (Pair (Int64 1L, Int64 99L)) script in
  let expected_value =
    Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 100L) in

  Alcotest.(check Testable.value) "Same value" expected_value result.storage;

  (* Check decrement *)
  let result =
    Vm_test.execute_ast_exn 6101 (Pair (Int64 0L, Int64 33L)) script in
  let expected_value =
    Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 32L) in

  Alcotest.(check Testable.value) "Same value" expected_value result.storage

let check_execution_error result expected_error =
  let open Vm_test in
  match result with
  | Error (Execution_error error) ->
    Alcotest.(check Testable.execution_error) "Same error" expected_error error
  | Ok _ -> Alcotest.fail "Ast shouldn't execute"
  | Error (Compilation_error error) ->
    Alcotest.fail (Vm_test.compilation_error_to_string error)

let check_compilation_error result expected_error =
  let open Vm_test in
  match result with
  | Error (Compilation_error error) ->
    Alcotest.(check Testable.compilation_error)
      "Same error" expected_error error
  | Ok _ -> Alcotest.fail "Ast shouldn't execute"
  | Error (Execution_error error) ->
    Alcotest.fail (Vm_test.execution_error_to_string error)

let test_compilation_undefined_variable () =
  let script = Ast.{ param = "_"; code = Var "x" } in
  check_compilation_error
    (Vm_test.execute_ast 2000 (Int64 0L) script)
    Undefined_variable

let test_fst_value_is_not_pair () =
  let script =
    Ast.
      {
        param = "_";
        code =
          Pair
            {
              first = Fst (Const 1L);
              second = Pair { first = Const 0L; second = Const 0L };
            };
      } in
  check_execution_error
    (Vm_test.execute_ast 2000 (Int64 0L) script)
    Value_is_not_pair

let test_snd_value_is_not_pair () =
  let script =
    Ast.
      {
        param = "_";
        code =
          Pair
            {
              first = Snd (Const 1L);
              second = Pair { first = Const 0L; second = Const 0L };
            };
      } in
  check_execution_error
    (Vm_test.execute_ast 2000 (Int64 0L) script)
    Value_is_not_pair

let test_op1_value_is_not_int64 () =
  let script =
    Ast.
      {
        param = "_";
        code =
          Pair
            {
              first =
                App
                  {
                    funct = Prim Neg;
                    arg = Pair { first = Const 0L; second = Const 0L };
                  };
              second = Pair { first = Const 0L; second = Const 0L };
            };
      } in
  check_execution_error
    (Vm_test.execute_ast 2000 (Int64 0L) script)
    Value_is_not_int64

let test_op2_value_is_not_int64 () =
  let script =
    Ast.
      {
        param = "_";
        code =
          Pair
            {
              first =
                App
                  {
                    funct =
                      App
                        {
                          funct = Prim Add;
                          arg = Pair { first = Const 0L; second = Const 0L };
                        };
                    arg = Const 0L;
                  };
              second = Pair { first = Const 0L; second = Const 0L };
            };
      } in
  check_execution_error
    (Vm_test.execute_ast 3000 (Int64 0L) script)
    Value_is_not_int64

let test_if_value_is_not_int64 () =
  let script =
    Ast.
      {
        param = "_";
        code =
          Pair
            {
              first =
                If
                  {
                    predicate = Pair { first = Const 0L; second = Const 0L };
                    consequent = Const 1L;
                    alternative = Const 1L;
                  };
              second = Pair { first = Const 0L; second = Const 0L };
            };
      } in
  check_execution_error
    (Vm_test.execute_ast 3000 (Int64 0L) script)
    Value_is_not_int64

let test_value_is_not_function () =
  let script =
    Ast.
      {
        param = "_";
        code =
          Pair
            {
              first = App { funct = Const 0L; arg = Const 0L };
              second = Pair { first = Const 0L; second = Const 0L };
            };
      } in
  check_execution_error
    (Vm_test.execute_ast 3000 (Int64 0L) script)
    Value_is_not_function

let test_pattern1_value_is_not_pair () =
  let script =
    Ast.{ param = "_"; code = Pair { first = Const 0L; second = Const 0L } }
  in
  check_execution_error
    (Vm_test.execute_ast 3000 (Int64 0L) script)
    Value_is_not_pair

let test_pattern2_value_is_not_pair () =
  let script = Ast.{ param = "_"; code = Const 0L } in
  check_execution_error
    (Vm_test.execute_ast 3000 (Int64 0L) script)
    Value_is_not_pair

let test_pattern3_value_is_not_zero () =
  let script =
    Ast.
      {
        param = "_";
        code =
          Pair
            {
              first = Const 0L;
              second = Pair { first = Const 1L; second = Const 0L };
            };
      } in
  check_execution_error
    (Vm_test.execute_ast 3000 (Int64 0L) script)
    Value_is_not_zero

let test_pattern4_value_is_not_zero () =
  let script =
    Ast.
      {
        param = "_";
        code =
          Pair
            {
              first = Const 0L;
              second = Pair { first = Const 0L; second = Const 1L };
            };
      } in
  check_execution_error
    (Vm_test.execute_ast 3000 (Int64 0L) script)
    Value_is_not_zero

let () =
  let open Alcotest in
  run "Lambda VM"
    [
      ( "Simple with simple expressions",
        [
          test_case "Increment" `Quick test_increment;
          test_case "Decrement" `Quick test_decrement;
          test_case "Add pair" `Quick test_add_pair;
          test_case "If expr" `Quick test_if_expr;
          test_case "Lambda" `Quick test_lambda;
        ] );
      ( "Compilation and execution errors",
        [
          test_case "Compilation - Undefined variable" `Quick
            test_compilation_undefined_variable;
          test_case "Fst - Value should be pair" `Quick
            test_fst_value_is_not_pair;
          test_case "Snd - Value should be pair" `Quick
            test_snd_value_is_not_pair;
          test_case "Op1 - Value should be int64" `Quick
            test_op1_value_is_not_int64;
          test_case "Op2 - Value should be int64" `Quick
            test_op2_value_is_not_int64;
          test_case "If - Value should be int64" `Quick
            test_if_value_is_not_int64;
          test_case "Value is not a function" `Quick test_value_is_not_function;
        ] );
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
        ] );
    ]
