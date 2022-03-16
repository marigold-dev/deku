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
