open Setup
open Lambda_vm
open Ast

exception Execution_error of execution_error

let expect_ir_value =
  Alcotest.(
    check
      (testable
         (fun ppf value -> Fmt.pf ppf "%s" (Ir.show_value value))
         Ir.equal_value))

let test_lowercase () =
  Alcotest.(check string)
    "same string" "hello!"
    (String.lowercase_ascii "hELLO!")

let expect_script_output ~script ~parameter ~expectation description =
  let script = script |> compile (Gas.make ~initial_gas:100000) in
  let parameter = parameter |> compile_value (Gas.make ~initial_gas:100000) in
  match (script, parameter) with
  | Ok script, Ok parameter -> (
    let script_result =
      script |> execute (Gas.make ~initial_gas:100000) ~arg:parameter in
    match script_result with
    | Ok { storage; operations = _ } ->
      expect_ir_value description storage expectation
    | Error Undefined_variable -> failwith "undefined variable in script result"
    | Error Over_applied_primitives ->
      failwith "over applied primitives in script result"
    (* user program bugs *)
    | Error Value_is_not_pair -> failwith "value is not pair"
    | Error Value_is_not_int64 -> failwith "value is not int64"
    | Error Value_is_not_function -> failwith "value is not function"
    | Error Value_is_not_zero -> failwith "value is not zero")
  | Error Undefined_variable, _ -> failwith "undefined variable in script"
  | _, Error Undefined_variable -> failwith "undefined variable in parameter"

let make_test description test =
  (description, `Quick, fun () -> test description)

let lam x f = Lam (x, f (Var x))
let lam2 x y f = Lam (x, Lam (y, f (Var x) (Var y)))
let script x f = { param = x; code = f (Var x) }
let pair x y : Ast.expr = Ast.Pair { first = x; second = y }
let app f ls =
  List.fold_left (fun acc x -> Ast.App { funct = acc; arg = x }) f ls
let let_in letins x =
  let let_in (var', let') in' = App { funct = Lam (var', in'); arg = let' } in
  List.fold_right let_in letins x

let make_pair =
  make_test "make_pair"
    (expect_script_output
       ~script:
         (script "y" (fun y ->
              app (lam "x" (fun x -> pair x (pair (Const 0L) (Const 0L)))) [y]))
       ~parameter:(Ast.Int64 44L) ~expectation:(Ir.V_int64 44L))

let make_pair_with_useless_lambda =
  make_test "make_pair_with_useless_lambda"
    (expect_script_output
       ~script:
         (script "y" (fun y ->
              app
                (lam2 "x" "y" (fun x _ -> pair x (pair (Const 0L) (Const 0L))))
                [y; Const 0L]))
       ~parameter:(Ast.Int64 44L) ~expectation:(Ir.V_int64 44L))
let make_pair_with_useless_lambda' =
  make_test "make_pair_with_useless_lambda'"
    (expect_script_output
       ~script:
         (script "y" (fun y ->
              app
                (lam2 "x" "y" (fun _ x -> pair x (pair (Const 0L) (Const 0L))))
                [Const 0L; y]))
       ~parameter:(Ast.Int64 44L) ~expectation:(Ir.V_int64 44L))

let add =
  make_test "add"
    (expect_script_output
       ~script:
         (script "y" (fun y ->
              app
                (lam2 "x" "y" (fun x z ->
                     pair (app (Prim Add) [x; z]) (pair (Const 0L) (Const 0L))))
                [Const 10L; y]))
       ~parameter:(Ast.Int64 44L) ~expectation:(Ir.V_int64 54L))

let fst =
  make_test "fst"
    (expect_script_output
       ~script:
         (script "y" (fun y ->
              app
                (lam "x" (fun x -> pair (Fst x) (pair (Const 0L) (Const 0L))))
                [y]))
       ~parameter:(Ast.Pair (Int64 33L, Int64 55L))
       ~expectation:(Ir.V_int64 33L))

let snd =
  make_test "snd"
    (expect_script_output
       ~script:
         (script "y" (fun y ->
              app
                (lam "x" (fun x -> pair (Snd x) (pair (Const 0L) (Const 0L))))
                [y]))
       ~parameter:(Ast.Pair (Int64 33L, Int64 55L))
       ~expectation:(Ir.V_int64 55L))

let letin =
  make_test "letin"
    (expect_script_output
       ~script:
         (script "y" (fun y ->
              let_in
                [
                  ( "makepair",
                    lam "x" (fun x -> pair (Snd x) (pair (Const 0L) (Const 0L)))
                  );
                ]
                (app (Var "makepair") [y])))
       ~parameter:(Ast.Pair (Int64 33L, Int64 55L))
       ~expectation:(Ir.V_int64 55L))
let letin' =
  make_test "letin'"
    (expect_script_output
       ~script:
         (script "y" (fun y ->
              let_in
                [
                  ( "makepair_old",
                    lam "x" (fun x -> pair (Snd x) (pair (Const 0L) (Const 0L)))
                  );
                  ( "makepair",
                    Var "makepair_old"
                  );
                ]
                (app (Var "makepair") [y])))
       ~parameter:(Ast.Pair (Int64 33L, Int64 55L))
       ~expectation:(Ir.V_int64 55L))


       
let () =
  let open Alcotest in
  run "lambdavm"
    [
      ( "basics",
        [
          make_pair;
          make_pair_with_useless_lambda;
          make_pair_with_useless_lambda';
          add;
          fst;
          snd;
          letin;
          letin';
        ] );
    ]

let () =
  describe "lambdavm" (fun { test; _ } ->
      let test name f =
        test name (fun { expect; _ } ->
            let expect_script_result script parameter result =
              let script = script |> compile (Gas.make ~initial_gas:100000) in
              let parameter =
                parameter |> compile_value (Gas.make ~initial_gas:100000) in
              (expect.result script).toBeOk ();
              (expect.result parameter).toBeOk ();
              match (script, parameter) with
              | Ok script, Ok parameter -> (
                let script_result =
                  script |> execute (Gas.make ~initial_gas:1000) ~arg:parameter
                in
                match script_result with
                | Ok { storage; operations = _ } ->
                  expect.equal ~equals:equal_value storage result
                | Error Undefined_variable ->
                  failwith "undefined variable in script result"
                | Error Over_applied_primitives ->
                  failwith "over applied primitives in script result"
                (* user program bugs *)
                | Error Value_is_not_pair -> failwith "value is not pair"
                | Error Value_is_not_int64 -> failwith "value is not int64"
                | Error Value_is_not_function ->
                  failwith "value is not function"
                | Error Value_is_not_zero -> failwith "value is not zero")
              | Error Undefined_variable, _ ->
                failwith "undefined variable in script"
              | _, Error Undefined_variable ->
                failwith "undefined variable in parameter" in
            f expect expect_script_result) in
      let open Ast in
      let lam x f = Lam (x, f (Var x)) in
      test "yellow" (fun expect _ -> expect.equal 0 1);
      test "blue" (fun _ _ -> failwith "hmm");
      test "red" (fun expect expect_ir ->
          (expect.fn (fun _ ->
               expect_ir
                 {
                   param = "y";
                   code = App { funct = lam "x" (fun x -> x); arg = Var "y" };
                 }
                 (Ast.Int64 44L) (Ir.V_int64 45L)))
            .toThrowException
            (Execution_error Value_is_not_pair));
      test "orange" (fun expect _ ->
          expect.equal ~equals:equal_value (Ir.V_int64 45L) (Ir.V_int64 45L))
      (*let _lam2 x y f = Lam (x, Lam (y, f (Var x) (Var y))) in
        test "lambda application" (fun _ expect_script_result ->
            let script =
              {
                param = "y";
                code =
                  App
                    {
                      funct =
                        lam "x" (fun x ->
                            Pair
                              {
                                first = x;
                                second =
                                  Pair { first = Const 0L; second = Const 0L };
                              });
                      arg = Var "y";
                    };
              } in
            let parameter = Ast.Int64 44L in
            let result = Ir.V_int64 45L in
            expect_script_result script parameter result);

        let () =
          test "add" (fun _ expect_script_result ->
              let script =
                {
                  param = "y";
                  code =
                    App
                      {
                        funct =
                          lam "y" (fun y ->
                              App
                                {
                                  funct =
                                    App { funct = Prim Add; arg = Const 32L };
                                  arg = y;
                                });
                        arg = Var "y";
                      };
                } in
              let parameter = Ast.Int64 45L in
              let result = Ir.V_int64 0L in
              expect_script_result script parameter result) in
        let stdlib code =
          let open Ast in
          let let_in (var', let') in' =
            App { funct = Lam (var', in'); arg = let' } in
          let funs =
            [
              ("id", lam "x" (fun x -> x))
              (*
                 Bignum data format format is (Int, (Int * (Int * ...)))
                                               ╰┬╯  ╰────────┬────────╯
                                               limb        number

                 Limb is the number of ints that are used to represent the number.
                 Number is a rope of tuples, each one more significant than the last

                 Limb is always at least 1.
              *)
              (*( "add_bignat",
                lam2 "a" "b" (fun _a _b ->
                    If
                      {
                        predicate = assert false;
                        consequent = assert false;
                        alternative = assert false;
                      }) )
                ("sub_bignat", lam2 "x" "y" (fun x y -> x - y));
                ("mul_bignat", lam2 "x" "y" (fun x y -> x * y));
                ("div_bignat", lam2 "x" "y" (fun x y -> x / y));
                ("mod_bignat", lam2 "x" "y" (fun x y -> x % y));
                ("eq_bignat", lam2 "x" "y" (fun x y -> x = y));
                ("lt_bignat", lam2 "x" "y" (fun x y -> x < y));
                ("gt_bignat", lam2 "x" "y" (fun x y -> x > y));
                ("le_bignat", lam2 "x" "y" (fun x y -> x <= y));
                ("ge_bignat", lam2 "x" "y" (fun x y -> x >= y));
                ("ne_bignat", lam2 "x" "y" (fun x y -> x != y));*);
            ] in
          List.fold_right let_in funs code in
        test "stdlibtest" (fun _ _expect_script_result ->
            let _script =
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
                } in
            let _parameter = Ast.Int64 45L in
            let _result = failwith "???" in
            ())*))
