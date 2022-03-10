open Setup
open Lambda_vm

let () =
  describe "lambdavm" (fun { test; _ } ->
      let test name f =
        test name (fun { expect; _ } ->
            let expect_ir a b = expect.equal ~equals:equal_value a b in
            f expect expect_ir) in
      let () =
        test "lambda application" (fun _ expect_ir ->
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
                                  second =
                                    Pair { first = Const 0L; second = Const 0L };
                                } );
                        arg = Var "y";
                      };
                }
              |> compile (Gas.make ~initial_gas:1000) in
            let parameter =
              Ast.Int64 45L |> compile_value (Gas.make ~initial_gas:1000) in
            match (script, parameter) with
            | Ok script, Ok parameter -> (
              let script_result =
                script |> execute (Gas.make ~initial_gas:1000) ~arg:parameter
              in
              match script_result with
              | Ok { storage; operations = _ } -> expect_ir storage parameter
              | Error Undefined_variable ->
                failwith "undefined variable in script result"
              | Error Over_applied_primitives ->
                failwith "over applied primitives in script result"
              (* user program bugs *)
              | Error Value_is_not_pair -> failwith "value is not pair"
              | Error Value_is_not_int64 -> failwith "value is not int64"
              | Error Value_is_not_function -> failwith "value is not function"
              | Error Value_is_not_zero -> failwith "value is not zero")
            | Error Undefined_variable, _ ->
              failwith "undefined variable in script"
            | _, Error Undefined_variable ->
              failwith "undefined variable in parameter") in
      let stdlib code =
        let open Ast in
        let let_in (var', let') in' =
          App { funct = Lam (var', in'); arg = let' } in
        let lam x f = Lam (x, f (Var x)) in
        let funs = [("id", lam "x" (fun x -> x))] in
        List.fold_right let_in funs code in
      test "stdlibtest" (fun _ expect_ir ->
          let script =
            Ast.
              {
                param = "y";
                code =
                  stdlib
                    (App
                       {
                         funct =
                           Lam
                             ( "x",
                               Pair
                                 {
                                   first = Var "x";
                                   second =
                                     Pair
                                       { first = Const 0L; second = Const 0L };
                                 } );
                         arg = Var "y";
                       });
              }
             in
          let script = script |> compile (Gas.make ~initial_gas:1000000) in
          let parameter =
            Ast.Int64 45L |> compile_value (Gas.make ~initial_gas:1000000) in
          match (script, parameter) with
          | Ok script, Ok parameter -> (
            let script_result =
              script |> execute (Gas.make ~initial_gas:1000000) ~arg:parameter
            in
            match script_result with
            | Ok { storage; operations = _ } -> expect_ir storage parameter
            | Error Undefined_variable ->
              failwith "undefined variable in script result"
            | Error Over_applied_primitives ->
              failwith "over applied primitives in script result"
            (* user program bugs *)
            | Error Value_is_not_pair -> failwith "value is not pair"
            | Error Value_is_not_int64 -> failwith "value is not int64"
            | Error Value_is_not_function -> failwith "value is not function"
            | Error Value_is_not_zero -> failwith "value is not zero")
          | Error Undefined_variable, _ ->
            failwith "undefined variable in script"
          | _, Error Undefined_variable ->
            failwith "undefined variable in parameter"))
