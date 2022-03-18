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
  id : Ast.expr -> Ast.expr;
  lower_bits : Ast.expr -> Ast.expr;
  higher_bits : Ast.expr -> Ast.expr;
  add_with_carry : Ast.expr -> Ast.expr -> Ast.expr;
  uncurry : Ast.expr -> Ast.expr -> Ast.expr;
  to_bignat : Ast.expr -> Ast.expr;
}
let stdlib x =
  let mklam name x = app (Var name) [x] in
  let mklam2 name x y = app (Var name) [x; y] in
  let ( lsl ) x y = app (Prim Lsl) [x; Const (Int64.of_int y)] in
  let ( lsr ) x y = app (Prim Lsr) [x; Const (Int64.of_int y)] in
  let* _id = ("id", lam "x" (fun x -> x)) in
  let id x = app (Var "id") [x] in

  let* _lower_bits = ("lower_bits", lam "x" (fun x -> (x lsl 32) lsr 32)) in
  let* _higher_bits = ("higher_bits", lam "x" (fun x -> x lsr 32)) in
  let lower_bits = mklam "lower_bits" in
  let higher_bits = mklam "higher_bits" in
  let* _add_with_carry =
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
          pair z c) ) in
  let add_with_carry = mklam2 "add_with_carry" in
  let* _uncurry =
    ("uncurry", lam2 "f" "tup" (fun f tup -> app f [fst tup; snd tup])) in
  let uncurry = mklam2 "uncurry" in
  let* _to_bignat = ("to_bignat", lam "x" (fun x -> pair (Const 0L) x)) in
  let to_bignat = mklam "to_bignat" in
  let* _add_bignat =
    ("add_bignat", lam2 "x" "y" (fun x y -> add_with_carry x y)) in
  x { id; lower_bits; higher_bits; add_with_carry; uncurry; to_bignat }

let test_stdlib =
  let test_stdlib_function description f ~parameter ~expectation =
    make_test description
      (expect_script_output
         ~script:
           (script "y" (fun y ->
                stdlib (fun r ->
                    app
                      (lam "x" (fun x ->
                           pair ((f r) x) (pair (Const 0L) (Const 0L))))
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
                             ((f r) (fst x) (snd x))
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
    test_stdlib_function "to_bignat"
      (fun { to_bignat; _ } -> to_bignat)
      ~parameter:(Ast.Int64 44L)
      ~expectation:
        (Ir.V_pair { first = Ir.V_int64 0L; second = Ir.V_int64 44L });
  ]

let add_with_carry =
  let expect_int64_list =
    Alcotest.(
      check
        (testable
           (fun ppf value ->
             Fmt.pf ppf "%s" ([%derive.show: Int64.t list] value))
           [%derive.eq: Int64.t list])) in
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
    make_test "Math.triple_add_with_carry maxint maxint maxint (yes overflow)"
      (fun description ->
        expect_int64_pair description (-3L, 2L)
          (Math.triple_add_with_carry (-1L) (-1L) (-1L)));
    (* Should never happen, but might as well test it anyway *)
    make_test "Math.add_bignum empty" (fun description ->
        expect_int64_list description [] (Math.add_bignum [] []));
    make_test "Math.add_bignum empty zeros" (fun description ->
        expect_int64_list description [0L] (Math.add_bignum [0L] [0L]));
    make_test "Math.add_bignum maxint maxint" (fun description ->
        expect_int64_list description [-2L; 1L] (Math.add_bignum [-1L] [-1L]));
    make_test "Math.add_bignum maxint^2 1" (fun description ->
        expect_int64_list description [0L; 0L; 1L]
          (Math.add_bignum [1L] [-1L; -1L]));
    (* result of this one is:
       0111111111111111111111111111111111111111111111111111111111111111  = -2L
       1111111111111111111111111111111111111111111111111111111111111111  = -1L
       1111111111111111111111111111111111111111111111111111111111111111  = -1L
       1000000000000000000000000000000000000000000000000000000000000000  = 1L
       (little endian)
    *)
    make_test "Math.add_bignum maxint^3  maxint^3" (fun description ->
        expect_int64_list description [-2L; -1L; -1L; 1L]
          (Math.add_bignum [-1L; -1L; -1L] [-1L; -1L; -1L]));
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

(*
    
        [4, 6, 10, 14, 22, 26, 34, 38, 46, 58, 62, 70, 74, 82, 86, 94, 98, 106,
        110, 118, 122, 130, 134, 142, 146, 154, 158, 166, 170, 178, 182, 190,
        194, 202, 206, 214, 218, 226, 230, 238, 242, 250, 254, 262, 266, 274,
        278, 286, 290, 298, 302, 310, 314, 322, 326, 334, 338, 346, 350, 358,
        362, 370, 374, 382, 386, 394, 398, 406, 410, 418, 422, 430, 434, 442,
        446, 454, 458, 466, 470, 478, 482, 490, 494, 502, 506, 514, 518, 526,
        530, 538, 542, 550, 554, 562, 566, 574, 578, 586, 590, 598, 602, 610,
        614, 622, 626, 634, 638, 646, 650, 658, 662, 670, 674, 682, 686, 694,
        698, 706, 710, 718, 722, 730, 734, 742, 746, 754, 758, 766, 770, 778,
        782, 790, 794, 802, 806, 814, 818, 826, 830, 838, 842, 850, 854, 862,
        866, 874, 878, 886, 890, 898, 902, 910, 914, 922, 926, 934, 938, 946,
        950, 958, 962, 970, 974, 982, 986, 994, 998, 1006, 1010, 1018, 1022,
        1030, 1034, 1042, 1046, 1054, 1058, 1066, 1070, 1078, 1082, 1090, 1094,
        1102, 1106, 1114, 1118, 1126, 1130, 1138, 1142, 1150, 1154,

    *)
