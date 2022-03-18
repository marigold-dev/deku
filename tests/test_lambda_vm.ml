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

let expect_script_output ~script:script' ~parameter ~expectation description =
  let script = script' |> compile (Gas.make ~initial_gas:100000) in
  let parameter = parameter |> compile_value (Gas.make ~initial_gas:100000) in
  match (script, parameter) with
  | Ok script, Ok parameter -> (
    let script_result =
      script |> execute (Gas.make ~initial_gas:100000) ~arg:parameter in
    match script_result with
    | Ok { storage; operations = _ } ->
      expect_ir_value description expectation storage
    | Error Undefined_variable -> failwith "undefined variable in script result"
    | Error Over_applied_primitives ->
      failwith "over applied primitives in script result"
    (* user program bugs *)
    | Error Value_is_not_pair -> failwith "value is not pair"
    | Error Value_is_not_int64 -> failwith "value is not int64"
    | Error Value_is_not_function -> failwith "value is not function"
    | Error Value_is_not_zero -> failwith "value is not zero")
  | Error Undefined_variable, _ ->
    failwith
      (String.concat "\n"
         ["undefined variable in script:"; Ast.show_script script'])
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
let ( let* ) (var', let') in' =
  App { funct = Lam (var', in' (Var var')); arg = let' }
let let' var ~eq ~in' = App { funct = Lam (var, in' (Var var)); arg = eq }
let fst x = Ast.(App { funct = Prim Fst; arg = x })
let snd x = Ast.(App { funct = Prim Snd; arg = x })
let ( + ) x y = app (Prim Add) [x; y]

let pair_wrapper f =
  script "y" (fun y ->
      app
        (lam "x" (fun _ -> pair (f (Var "x")) (pair (Const 0L) (Const 0L))))
        [y])

let make_pair =
  make_test "make_pair"
    (expect_script_output
       ~script:(pair_wrapper (fun x -> x))
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
       ~script:(pair_wrapper (fun x -> fst x))
       ~parameter:(Ast.Pair (Int64 33L, Int64 55L))
       ~expectation:(Ir.V_int64 33L))

let test_snd =
  make_test "snd"
    (expect_script_output
       ~script:(pair_wrapper (fun x -> snd x))
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
let monadic_let =
  make_test "monadic_let"
    (expect_script_output
       ~script:
         (pair_wrapper (fun y ->
              let* res = ("res", y + Const 10L) in
              res + Const 5L))
       ~parameter:(Int64 33L) ~expectation:(Ir.V_int64 48L))

type stdlib = {
  id : Ast.expr;
  lower_bits : Ast.expr;
  higher_bits : Ast.expr;
  add_with_carry : Ast.expr;
}
let stdlib x =
  let open Ast in
  let ( lsl ) x y = app (Prim Lsl) [x; Const (Int64.of_int y)] in
  let ( lsr ) x y = app (Prim Lsr) [x; Const (Int64.of_int y)] in
  let lower_bits x = app (Var "lower_bits") [x] in
  let higher_bits x = app (Var "higher_bits") [x] in
  let_in
    [
      ("id", lam "x" (fun x -> x));
      ("lower_bits", lam "x" (fun x -> (x lsl 32) lsr 32));
      ("higher_bits", lam "x" (fun x -> x lsr 32));
      ( "add_with_carry",
        lam2 "x" "y" (fun x y ->
            let* lx = ("lx", lower_bits x) in
            let* ly = ("ly", lower_bits y) in
            let* lz_lc = ("lz_lc", lx + ly) in
            let* lz = ("lz", lower_bits lz_lc) in
            (* lower carry *)
            let* lc = ("lc", higher_bits lz_lc) in

            let* hx = ("hx", higher_bits x) in
            let* hy = ("hy", higher_bits y) in
            let* hz_hc = ("hz_hc", hx + hy + lc) in
            let* hz = ("hz", lower_bits hz_hc) in
            (* higher carry *)
            let* hc = ("hc", higher_bits hz_hc) in
            let* z = ("z", (hz lsl 32) + lz) in
            let* c = ("c", hc) in
            pair z c) );
    ]
    (x
       Ast.
         {
           id = Var "id";
           lower_bits = Var "lower_bits";
           higher_bits = Var "higher_bits";
           add_with_carry = Var "add_with_carry";
         })

let test_stdlib =
  let test_stdlib_function description f ~parameter ~expectation =
    make_test description
      (expect_script_output
         ~script:
           (script "y" (fun y ->
                stdlib (fun r ->
                    app
                      (lam "x" (fun x ->
                           pair (app (f r) [x]) (pair (Const 0L) (Const 0L))))
                      [y])))
         ~parameter ~expectation) in
  let test_stdlib_function2 description f ~parameter ~expectation =
    make_test description
      (expect_script_output
         ~script:
           (script "y" (fun y ->
                stdlib (fun r ->
                    app
                      (lam "x" (fun x ->
                           pair
                             (app (f r) [fst x; snd x])
                             (pair (Const 0L) (Const 0L))))
                      [y])))
         ~parameter ~expectation) in
  [
    test_stdlib_function "id"
      (fun { id; _ } -> id)
      ~parameter:(Ast.Int64 44L) ~expectation:(Ir.V_int64 44L);
    test_stdlib_function "lower_bits"
      (fun { lower_bits; _ } -> lower_bits)
      ~parameter:
        (Ast.Int64
           0b00000000000000000000000000000000110000000000000000000000000000000L)
      ~expectation:
        (Ir.V_int64
           0b00000000000000000000000000000000010000000000000000000000000000000L);
    test_stdlib_function "higher_bits"
      (fun { higher_bits; _ } -> higher_bits)
      ~parameter:
        (Ast.Int64
           0b00000000000000000000000000000000110000000000000000000000000000000L)
      ~expectation:(Ir.V_int64 0b000000000000000000000000000000001L);
    test_stdlib_function2 "add_with_carry 33 11"
      (fun { add_with_carry; _ } -> add_with_carry)
      ~parameter:(Ast.Pair (Int64 33L, Int64 11L))
      ~expectation:
        (Ir.V_pair { first = Ir.V_int64 44L; second = Ir.V_int64 0L });
    test_stdlib_function2 "add_with_carry maxint 0"
      (fun { add_with_carry; _ } -> add_with_carry)
      ~parameter:(Ast.Pair (Int64 (-1L), Int64 0L))
      ~expectation:
        (Ir.V_pair { first = Ir.V_int64 (-1L); second = Ir.V_int64 0L });
    test_stdlib_function2 "add_with_carry maxint 1"
      (fun { add_with_carry; _ } -> add_with_carry)
      ~parameter:(Ast.Pair (Int64 (-1L), Int64 1L))
      ~expectation:(Ir.V_pair { first = Ir.V_int64 0L; second = Ir.V_int64 1L });
    test_stdlib_function2 "add_with_carry maxint maxint"
      (fun { add_with_carry; _ } -> add_with_carry)
      ~parameter:(Ast.Pair (Int64 (-1L), Int64 (-1L)))
      ~expectation:
        (Ir.V_pair { first = Ir.V_int64 (-2L); second = Ir.V_int64 1L });
  ]

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
          (Math.add_with_carry (-1L) (-1L)))
    (*
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
           (Ir.V_pair { first = Ir.V_int64 55L; second = Ir.V_int64 0L }));*);
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
          monadic_let;
        ] );
      ("stdlib", test_stdlib);
      ("math", List.concat [add_with_carry]);
    ]
