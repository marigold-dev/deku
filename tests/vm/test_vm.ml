open Lambda_vm

module Vm_test = struct
  type error =
    | Compilation_error of compile_error
    | Execution_error   of execution_error

  let pp_to_string f =
    let buf = Buffer.create 20 in
    let fmt = Format.formatter_of_buffer buf in
    f fmt;
    Buffer.contents buf

  let compilation_error_to_string error =
    pp_to_string (fun fmt -> pp_compile_error fmt error)

  let execution_error_to_string error =
    pp_to_string (fun fmt -> pp_execution_error fmt error)

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
    match (compile_value gas arg, compile gas script) with
    | Ok arg, Ok ir -> (
      match execute gas ~arg ir with
      | Ok result -> Ok result
      | Error error -> Error (Execution_error error))
    | Error error, _
    | _, Error error ->
      Error (Compilation_error error)
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

module Simple_expressions = struct
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

  let test_lambda_application () =
    let script =
      Ast.
        {
          param = "y";
          code =
            App
              {
                funct =
                  Lam
                    ( "x",
                      Pair
                        {
                          first = Var "x";
                          second = Pair { first = Const 0L; second = Const 0L };
                        } );
                arg = Var "y";
              };
        } in
    let result = Vm_test.execute_ast_exn 2000 (Int64 45L) script in
    let expected_value =
      Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 45L) in

    Alcotest.(check Testable.value) "Same value" expected_value result.storage
end

module Compilation_and_execution_errors = struct
  let check_execution_error result expected_error =
    let open Vm_test in
    match result with
    | Error (Execution_error error) ->
      Alcotest.(check Testable.execution_error)
        "Same error" expected_error error
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
      (Vm_test.execute_ast 1001 (Int64 0L) script)
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
      (Vm_test.execute_ast 1001 (Int64 0L) script)
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
      (Vm_test.execute_ast 1601 (Int64 0L) script)
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
      (Vm_test.execute_ast 2001 (Int64 0L) script)
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
      (Vm_test.execute_ast 1601 (Int64 0L) script)
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
      (Vm_test.execute_ast 1201 (Int64 0L) script)
      Value_is_not_function

  let test_pattern1_value_is_not_pair () =
    let script =
      Ast.{ param = "_"; code = Pair { first = Const 0L; second = Const 0L } }
    in
    check_execution_error
      (Vm_test.execute_ast 701 (Int64 0L) script)
      Value_is_not_pair

  let test_pattern2_value_is_not_pair () =
    let script = Ast.{ param = "_"; code = Const 0L } in
    check_execution_error
      (Vm_test.execute_ast 301 (Int64 0L) script)
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
      (Vm_test.execute_ast 1101 (Int64 0L) script)
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
      (Vm_test.execute_ast 1101 (Int64 0L) script)
      Value_is_not_zero
end

module Primitive_operations = struct
  let script_op2 prim =
    Ast.
      {
        param = "param";
        code =
          Pair
            {
              first =
                App
                  {
                    funct = App { funct = Prim prim; arg = Fst (Var "param") };
                    arg = Snd (Var "param");
                  };
              second = Pair { first = Const 0L; second = Const 0L };
            };
      }

  let script_op1 prim =
    Ast.
      {
        param = "param";
        code =
          Pair
            {
              first = App { funct = Prim prim; arg = Var "param" };
              second = Pair { first = Const 0L; second = Const 0L };
            };
      }

  let make_op2_test ~name prim f =
    QCheck_alcotest.to_alcotest
      QCheck.(
        Test.make ~name ~count:10_000 (pair int64 int64) (fun (a, b) ->
            let result =
              Vm_test.execute_ast_exn 2501
                (Pair (Int64 a, Int64 b))
                (script_op2 prim) in
            let expected_result =
              Vm_test.compile_value_exn
                (Gas.make ~initial_gas:200)
                (Int64 (f a b)) in
            result.storage = expected_result))

  let test_sum = make_op2_test ~name:"Adding pairs" Add Int64.add

  let test_sub = make_op2_test ~name:"Subtracting pairs" Sub Int64.sub

  let test_mul = make_op2_test ~name:"Multiplying pairs" Mul Int64.mul

  let test_div = make_op2_test ~name:"Dividing pairs" Div Int64.div

  let test_rem = make_op2_test ~name:"Modulo pairs" Rem Int64.rem

  let test_and = make_op2_test ~name:"Anding pairs" And Int64.logand

  let test_or = make_op2_test ~name:"Oring pairs" Or Int64.logor

  let test_xor = make_op2_test ~name:"Xoring pairs" Xor Int64.logxor

  let test_lsl =
    make_op2_test ~name:"Left shifting pairs" Lsl (fun a b ->
        Int64.shift_left a (Int64.to_int b))

  let test_lsr =
    make_op2_test ~name:"Logical right shifting pairs" Lsr (fun a b ->
        Int64.shift_right_logical a (Int64.to_int b))

  let test_asr =
    make_op2_test ~name:"Right shifting pairs" Asr (fun a b ->
        Int64.shift_right a (Int64.to_int b))

  let test_neg =
    QCheck_alcotest.to_alcotest
      QCheck.(
        Test.make ~name:"Negative numbers" ~count:10_000 int64 (fun x ->
            let result =
              Vm_test.execute_ast_exn 1501 (Int64 x) (script_op1 Neg) in
            let expected_result =
              Vm_test.compile_value_exn
                (Gas.make ~initial_gas:200)
                (Int64 Int64.(neg x)) in
            result.storage = expected_result))
end

module Recursion = struct
  let factorial =
    Ast.
      {
        param = "x";
        code =
          Pair
            {
              first =
                App
                  {
                    funct =
                      Lam
                        ( "f",
                          App
                            {
                              funct = App { funct = Var "f"; arg = Var "f" };
                              arg = Var "x";
                            } );
                    arg =
                      Lam
                        ( "f",
                          Lam
                            ( "n",
                              If
                                {
                                  predicate = Var "n";
                                  consequent =
                                    App
                                      {
                                        funct =
                                          App
                                            { funct = Prim Mul; arg = Var "n" };
                                        arg =
                                          App
                                            {
                                              funct =
                                                App
                                                  {
                                                    funct = Var "f";
                                                    arg = Var "f";
                                                  };
                                              arg =
                                                App
                                                  {
                                                    funct =
                                                      App
                                                        {
                                                          funct = Prim Sub;
                                                          arg = Var "n";
                                                        };
                                                    arg = Const 1L;
                                                  };
                                            };
                                      };
                                  alternative = Const 1L;
                                } ) );
                  };
              second = Pair { first = Const 0L; second = Const 0L };
            };
      }

  let test_factorial =
    let rec fac = function
      | 0L -> 1L
      | n -> Int64.(mul n (fac (sub n 1L))) in
    QCheck_alcotest.to_alcotest
      QCheck.(
        Test.make ~name:"Recursion with factorial" ~count:10000 (1 -- 26)
          (fun x ->
            (* Less than 0 is infinite recursion, greater than 25 is integer overflow. *)
            let x = Int64.of_int x in
            let result = Vm_test.execute_ast_exn 1_000_000 (Int64 x) factorial in
            let expected_result =
              Vm_test.compile_value_exn
                (Gas.make ~initial_gas:101)
                (Int64 (fac x)) in
            expected_result = result.storage))

  let fibonacci =
    Ast.
      {
        param = "x";
        code =
          Pair
            {
              first =
                App
                  {
                    funct =
                      Lam
                        ( "f",
                          App
                            {
                              funct = App { funct = Var "f"; arg = Var "f" };
                              arg = Var "x";
                            } );
                    arg =
                      Lam
                        ( "f",
                          Lam
                            ( "n",
                              If
                                {
                                  predicate =
                                    (* (0 - n) * (1 - n) *)
                                    App
                                      {
                                        funct =
                                          App
                                            {
                                              funct = Prim Mul;
                                              arg =
                                                App
                                                  {
                                                    funct =
                                                      App
                                                        {
                                                          funct = Prim Sub;
                                                          arg = Const 0L;
                                                        };
                                                    arg = Var "n";
                                                  };
                                            };
                                        arg =
                                          App
                                            {
                                              funct =
                                                App
                                                  {
                                                    funct = Prim Sub;
                                                    arg = Const 1L;
                                                  };
                                              arg = Var "n";
                                            };
                                      };
                                  consequent =
                                    App
                                      {
                                        funct =
                                          App
                                            {
                                              funct = Prim Add;
                                              arg =
                                                App
                                                  {
                                                    funct =
                                                      App
                                                        {
                                                          funct = Var "f";
                                                          arg = Var "f";
                                                        };
                                                    arg =
                                                      App
                                                        {
                                                          funct =
                                                            App
                                                              {
                                                                funct = Prim Sub;
                                                                arg = Var "n";
                                                              };
                                                          arg = Const 1L;
                                                        };
                                                  };
                                            };
                                        arg =
                                          App
                                            {
                                              funct =
                                                App
                                                  {
                                                    funct = Var "f";
                                                    arg = Var "f";
                                                  };
                                              arg =
                                                App
                                                  {
                                                    funct =
                                                      App
                                                        {
                                                          funct = Prim Sub;
                                                          arg = Var "n";
                                                        };
                                                    arg = Const 2L;
                                                  };
                                            };
                                      };
                                  alternative = Const 1L;
                                } ) );
                  };
              second = Pair { first = Const 0L; second = Const 0L };
            };
      }

  let test_fibonacci =
    let rec fib = function
      | 0L
      | 1L ->
        1L
      | n -> Int64.add (fib (Int64.sub n 1L)) (fib (Int64.sub n 2L)) in
    QCheck_alcotest.to_alcotest
      QCheck.(
        Test.make ~name:"Fibonacci" ~count:100 (0 -- 25) (fun x ->
            let x = Int64.of_int x in
            let result =
              Vm_test.execute_ast_exn 100000000000 (Int64 x) fibonacci in
            let expected_value =
              Vm_test.compile_value_exn
                (Gas.make ~initial_gas:101)
                (Int64 (fib x)) in
            expected_value = result.storage))

  let counter =
    Ast.
      {
        param = "x";
        code =
          Pair
            {
              first =
                App
                  {
                    funct =
                      Lam
                        ( "f",
                          App
                            {
                              funct = App { funct = Var "f"; arg = Var "f" };
                              arg = Var "x";
                            } );
                    arg =
                      Lam
                        ( "f",
                          Lam
                            ( "n",
                              If
                                {
                                  predicate = Var "n";
                                  consequent =
                                    App
                                      {
                                        funct =
                                          App
                                            { funct = Prim Add; arg = Const 1L };
                                        arg =
                                          App
                                            {
                                              funct =
                                                App
                                                  {
                                                    funct = Var "f";
                                                    arg = Var "f";
                                                  };
                                              arg =
                                                App
                                                  {
                                                    funct =
                                                      App
                                                        {
                                                          funct = Prim Sub;
                                                          arg = Var "n";
                                                        };
                                                    arg = Const 1L;
                                                  };
                                            };
                                      };
                                  alternative = Const 0L;
                                } ) );
                  };
              second = Pair { first = Const 0L; second = Const 0L };
            };
      }

  let test_counter =
    QCheck_alcotest.to_alcotest
      QCheck.(
        Test.make ~name:"Counter" ~count:1000 (0 -- 10000) (fun x ->
            let x = Int64.of_int x in
            let result = Vm_test.execute_ast_exn 1000000000 (Int64 x) counter in
            let expected_value =
              Vm_test.compile_value_exn (Gas.make ~initial_gas:101) (Int64 x)
            in
            expected_value = result.storage))

  let test_stack_limit () =
    Alcotest.check_raises "Stack has a limit" Out_of_stack (fun () ->
        let _ =
          Vm_test.execute_ast_exn 71_990_801
            (Int64 19996L) (* Bare minimum close to the limit of 20k *)
            counter in
        ())
end

let () =
  let open Alcotest in
  run "Lambda VM"
    [
      ( "Simple with simple expressions",
        Simple_expressions.
          [
            test_case "Increment" `Quick test_increment;
            test_case "Decrement" `Quick test_decrement;
            test_case "Add pair" `Quick test_add_pair;
            test_case "If expr" `Quick test_if_expr;
            test_case "Lambda" `Quick test_lambda;
            test_case "Lambda application" `Quick test_lambda_application;
          ] );
      ( "Compilation and execution errors",
        Compilation_and_execution_errors.
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
            test_case "Value is not a function" `Quick
              test_value_is_not_function;
          ] );
      ( "Execution pattern errors",
        Compilation_and_execution_errors.
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
      ( "Primitive operations",
        Primitive_operations.
          [
            test_sum;
            test_sub;
            test_mul;
            test_div;
            test_rem;
            test_and;
            test_or;
            test_xor;
            test_neg;
            test_lsl;
            test_lsr;
            test_asr;
          ] );
      ( "Recursion",
        Recursion.
          [
            test_factorial;
            test_fibonacci;
            test_counter;
            test_case "Stack limit" `Slow test_stack_limit;
          ] );
    ]
