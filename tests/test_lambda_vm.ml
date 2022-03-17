open Lambda_vm
open Ast

exception Execution_error of execution_error

let expect_ir_value =
  Alcotest.(
    check
      (testable
         (fun ppf value -> Fmt.pf ppf "%s" (Ir.show_value value))
         Ir.equal_value))

let expect_uint64 =
  Alcotest.(
    check
      (testable
         (fun ppf value -> Fmt.pf ppf "%s" (Stdint.Uint64.to_string value))
         (fun a b -> Stdint.Uint64.compare a b == 0)))

let expect_int64 =
  Alcotest.(
    check
      (testable
         (fun ppf value -> Fmt.pf ppf "%s" (Int64.to_string value))
         Int64.equal))
let expect_uint128 =
  Alcotest.(
    check
      (testable
         (fun ppf value -> Fmt.pf ppf "%s" (Stdint.Uint128.to_string value))
         (fun a b -> Stdint.Uint128.compare a b == 0)))

let expect_int64_pair =
  Alcotest.(
    check
      (testable
         (fun ppf value ->
           Fmt.pf ppf "%s" ([%derive.show: Int64.t * Int64.t] value))
         [%derive.eq: Int64.t * Int64.t]))

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
let fst x = Ast.(App { funct = Prim Fst; arg = x })
let snd x = Ast.(App { funct = Prim Snd; arg = x })

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

let test_fst =
  make_test "fst"
    (expect_script_output
       ~script:
         (script "y" (fun y ->
              app
                (lam "x" (fun x -> pair (fst x) (pair (Const 0L) (Const 0L))))
                [y]))
       ~parameter:(Ast.Pair (Int64 33L, Int64 55L))
       ~expectation:(Ir.V_int64 33L))

let test_snd =
  make_test "snd"
    (expect_script_output
       ~script:
         (script "y" (fun y ->
              app
                (lam "x" (fun x -> pair (snd x) (pair (Const 0L) (Const 0L))))
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
                    lam "x" (fun x -> pair (snd x) (pair (Const 0L) (Const 0L)))
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
                    lam "x" (fun x -> pair (snd x) (pair (Const 0L) (Const 0L)))
                  );
                  ("makepair", Var "makepair_old");
                ]
                (app (Var "makepair") [y])))
       ~parameter:(Ast.Pair (Int64 33L, Int64 55L))
       ~expectation:(Ir.V_int64 55L))

let negative_uint_to_big_uint128 =
  make_test "negative uint to big_uint128" (fun description ->
      expect_uint128 description
        (Stdint.Uint64.max_int |> Math.Uint128.of_uint64)
        (Math.Uint128.of_int64 (-1L)))
let add_with_carry =
  [
    make_test "Math.add_with_carry 33 11 (no overflow)" (fun description ->
        expect_int64_pair description (44L, 0L) (Math.add_with_carry 33L 11L));
    (* Recall that -1L is treated as max_int64 *)
    make_test "Math.add_with_carry maxint 0 (no overflow)" (fun description ->
        expect_int64_pair description (-1L, 0L) (Math.add_with_carry (-1L) 0L));
    make_test "Math.add_with_carry maxint 1 (yes overflow)" (fun description ->
        expect_int64_pair description (0L, 1L) (Math.add_with_carry (-1L) 1L));
    make_test "Math.add_with_carry maxint maxint (yes overflow)"
      (fun description ->
        expect_int64_pair description (-2L, 1L)
          (Math.add_with_carry (-1L) (-1L)));
    make_test "interpreter add with carry"
      (expect_script_output
         ~script:
           (script "y" (fun y ->
                app
                  (lam "y" (fun y ->
                       pair
                         (app (Prim Add_with_carry) [fst y; snd y])
                         (pair (Const 0L) (Const 0L))))
                  [y]))
         ~parameter:(Ast.Pair (Ast.Int64 44L, Ast.Int64 11L))
         ~expectation:
           (Ir.V_pair { first = Ir.V_int64 55L; second = Ir.V_int64 0L }));
  ]

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
          test_fst;
          test_snd;
          letin;
          letin';
        ] );
      ("math", List.concat [[negative_uint_to_big_uint128]; add_with_carry]);
    ]
