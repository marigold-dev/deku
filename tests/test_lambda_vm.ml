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
         (fun a b -> Stdint.Uint64.compare a b = 0)))

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
         (fun a b -> Stdint.Uint128.compare a b = 0)))

let expect_int64_pair =
  Alcotest.(
    check
      (testable
         (fun ppf value ->
           Fmt.pf ppf "%s" ([%derive.show: Int64.t * Int64.t] value))
         [%derive.eq: Int64.t * Int64.t]))

let expect_script_output ~script:script' ~parameter ~expectation description =
  let initial_gas = 10000000 in
  let script = script' |> compile (Gas.make ~initial_gas) in
  let parameter = parameter |> compile_value (Gas.make ~initial_gas) in
  match (script, parameter) with
  | Ok script, Ok parameter -> (
    let script_result =
      script |> execute (Gas.make ~initial_gas) ~arg:parameter in
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
let lam3 x y z f = Lam (x, Lam (y, Lam (z, f (Var x) (Var y) (Var z))))
let lam4 w x y z f =
  Lam (w, Lam (x, Lam (y, Lam (z, f (Var w) (Var x) (Var y) (Var z)))))
let lam5 v w x y z f =
  Lam
    ( v,
      Lam
        (w, Lam (x, Lam (y, Lam (z, f (Var v) (Var w) (Var x) (Var y) (Var z)))))
    )
let lam6 u v w x y z f =
  Lam
    ( u,
      Lam
        ( v,
          Lam
            ( w,
              Lam
                ( x,
                  Lam
                    ( y,
                      Lam (z, f (Var u) (Var v) (Var w) (Var x) (Var y) (Var z))
                    ) ) ) ) )
let script x f = { param = x; code = f (Var x) }
let pair x y : Ast.expr = Ast.Pair { first = x; second = y }
let app f ls =
  List.fold_left (fun acc x -> Ast.App { funct = acc; arg = x }) f ls
let let_in letins x =
  let let_in (var', let') in' = App { funct = Lam (var', in'); arg = let' } in
  List.fold_right let_in letins x

let let' var ~eq ~in' = App { funct = Lam (var, in' (Var var)); arg = eq }
let fst x = Ast.(App { funct = Prim Fst; arg = x })
let snd x = Ast.(App { funct = Prim Snd; arg = x })
let if' cond ~then' ~else' =
  If { predicate = cond; consequent = then'; alternative = else' }
let ( + ) x y = app (Prim Add) [x; y]
let ( - ) x y = app (Prim Sub) [x; y]
let ( * ) x y = app (Prim Mul) [x; y]
let ( / ) x y = app (Prim Div) [x; y]

(* Technically the if is unecessary *)
let ( == ) x y = if' (x - y) ~then':(Const 0L) ~else':(Const 1L)
let ( <> ) x y = if' (x - y) ~then':(Const 1L) ~else':(Const 0L)
let ( && ) x y =
  if' (app (Prim Land) [x; y]) ~then':(Const 1L) ~else':(Const 0L)
let ( let* ) (var', let') in' =
  App { funct = Lam (var', in' (Var var')); arg = let' }
let ( let*& ) (var', var'', let') in' =
  let* v1 = (var', fst let') in
  let* v2 = (var'', snd let') in
  in' (v1, v2)

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
let monadic_bilet =
  make_test "monadic_let"
    (expect_script_output
       ~script:
         (pair_wrapper (fun y ->
              let*& v1, v2 = ("v1", "v2", y) in
              v1 + v2))
       ~parameter:(Pair (Int64 33L, Int64 11L))
       ~expectation:(Ir.V_int64 44L))

type stdlib = {
  id : Ast.expr -> Ast.expr;
  lower_bits : Ast.expr -> Ast.expr;
  higher_bits : Ast.expr -> Ast.expr;
  add_with_carry : Ast.expr -> Ast.expr -> Ast.expr;
  uncurry : (Ast.expr -> Ast.expr -> Ast.expr) -> Ast.expr -> Ast.expr;
  to_bignat : Ast.expr -> Ast.expr;
  add_bignat : Ast.expr -> Ast.expr -> Ast.expr;
  zcomb : Ast.expr -> Ast.expr;
}
let stdlib x =
  let mklam name x = app (Var name) [x] in
  let mklam2 name x y = app (Var name) [x; y] in
  let mklam3 name x y z = app (Var name) [x; y; z] in
  let _mklam4 name w x y z = app (Var name) [w; x; y; z] in
  let mklam5 name v w x y z = app (Var name) [v; w; x; y; z] in
  let ( lsl ) x y = app (Prim Lsl) [x; Const (Int64.of_int y)] in
  let ( lsr ) x y = app (Prim Lsr) [x; Const (Int64.of_int y)] in
  let* _id = ("id", lam "x" (fun x -> x)) in
  let id x = app (Var "id") [x] in

  let* _zcomb =
    ( "zcomb",
      lam "f" (fun f ->
          let d = lam "x" (fun x -> app f [lam "v" (fun v -> app x [x; v])]) in
          app d [d]) ) in
  let zcomb = mklam "zcomb" in
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
  (*let* _uncurry =
      ("uncurry", lam2 "f" "tup" (fun f tup -> app f [fst tup; snd tup])) in
    let uncurry = mklam2 "uncurry" in*)
  let uncurry f x = f (fst x) (snd x) in
  let* _to_bignat = ("to_bignat", lam "x" (fun x -> pair (Const 1L) x)) in
  let to_bignat = mklam "to_bignat" in
  let* _triple_add_with_carry =
    ( "triple_add_with_carry",
      lam3 "x" "y" "z" (fun x y z ->
          let* s1 = ("s1", add_with_carry x y) in
          let* s2 = ("s2", add_with_carry (fst s1) z) in
          pair (fst s2) (snd s1 + snd s2)) ) in
  let triple_add_with_carry = mklam3 "triple_add_with_carry" in
  let* _add_bignat =
    ( "add_bignat",
      lam2 "x" "y" (fun x y ->
          let*& x_limb, xs = ("x_limb", "xs", x) in
          let*& y_limb, ys = ("y_limb", "ys", y) in
let* add_bignat =
  ( "add_bignat",
    lam6 "recurse" "x_limb" "xs" "y_limb" "ys" "carry"
      (fun _recurse x_limb xs y_limb ys carry ->
        let add_bignat = mklam5 "recurse" in
        if'
          (x_limb <> Const 1L && y_limb <> Const 1L)
          ~then':
            (let*& x, xs = ("x", "xs", xs) in
              let*& y, ys = ("y", "ys", ys) in
              let*& l, carry =
                ("l", "carry", triple_add_with_carry x y carry) in
              let*& limb, ls =
                ( "limb",
                  "ls",
                  add_bignat (x_limb - Const 1L) xs (y_limb - Const 1L)
                    ys carry ) in
              pair (limb + Const 1L) (pair l ls))
          ~else':
            (if'
                (x_limb <> Const 1L && y_limb == Const 1L)
                ~then':
                  (let*& x, xs = ("x", "xs", xs) in
                  let*& l, carry =
                    ("l", "carry", add_with_carry x carry) in
                  let*& limb, ls =
                    ( "limb",
                      "ls",
                      (* this line is probably wrong *)
                      add_bignat (x_limb - Const 1L) xs (Const 0L)
                        (Const 0L) carry ) in
                  pair (limb + Const 1L) (pair l ls))
                ~else':(pair (Const 1L) (Const 0L)))) ) in
          let* _add_bignat = ("add_bignat", zcomb add_bignat) in
          let add_bignat = mklam5 "add_bignat" in
          add_bignat x_limb xs y_limb ys (Const 0L)) ) in
  let add_bignat = mklam2 "add_bignat" in
  x
    {
      id;
      lower_bits;
      higher_bits;
      add_with_carry;
      uncurry;
      to_bignat;
      zcomb;
      add_bignat;
    }

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
  let numlist_to_bignat_value pair i64 x =
    let rec makepairs = function
      | [x] -> i64 x
      | x :: xs -> pair (i64 x, makepairs xs)
      | _ ->
        failwith "`numlist_to_bignat_ast_value` caller passed an empty list"
    in
    pair (i64 (List.length x |> Int64.of_int), makepairs x) in
  let _numlist_to_bignat_ast_value =
    numlist_to_bignat_value
      (fun (x, y) -> Ast.Pair (x, y))
      (fun i -> Ast.Int64 i) in
  let _numlist_to_bignat_ir_value =
    numlist_to_bignat_value
      (fun (x, y) -> Ir.V_pair { first = x; second = y })
      (fun i -> Ir.V_int64 i) in
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
    test_stdlib_function2 "add_with_carry maxint64 0"
      (fun { add_with_carry; _ } -> add_with_carry)
      ~parameter:(Ast.Pair (Int64 (-1L), Int64 0L))
      ~expectation:
        (Ir.V_pair { first = Ir.V_int64 (-1L); second = Ir.V_int64 0L });
    test_stdlib_function2 "add_with_carry maxint64 1"
      (fun { add_with_carry; _ } -> add_with_carry)
      ~parameter:(Ast.Pair (Int64 (-1L), Int64 1L))
      ~expectation:(Ir.V_pair { first = Ir.V_int64 0L; second = Ir.V_int64 1L });
    test_stdlib_function2 "add_with_carry maxint64 maxint64"
      (fun { add_with_carry; _ } -> add_with_carry)
      ~parameter:(Ast.Pair (Int64 (-1L), Int64 (-1L)))
      ~expectation:
        (Ir.V_pair { first = Ir.V_int64 (-2L); second = Ir.V_int64 1L });
    test_stdlib_function "to_bignat"
      (fun { to_bignat; _ } -> to_bignat)
      ~parameter:(Ast.Int64 44L)
      ~expectation:
        (Ir.V_pair { first = Ir.V_int64 1L; second = Ir.V_int64 44L });
    test_stdlib_function "uncurry & eq"
      (fun { uncurry; _ } -> uncurry ( == ))
      ~parameter:(Ast.Pair (Int64 33L, Int64 44L))
      ~expectation:(Ir.V_int64 0L);
    test_stdlib_function "uncurry & eq'"
      (fun { uncurry; _ } -> uncurry ( == ))
      ~parameter:(Ast.Pair (Int64 33L, Int64 33L))
      ~expectation:(Ir.V_int64 1L);
    make_test "zcomb (factorial test)'"
      (expect_script_output
         ~script:
           (pair_wrapper (fun param ->
                stdlib (fun { zcomb; _ } ->
                    app
                      (zcomb
                         (lam2 "recurse" "n" (fun recurse n ->
                              if' (n == Const 0L) ~then':(Const 1L)
                                ~else':(n * app recurse [n - Const 1L]))))
                      [param])))
         ~parameter:(Ast.Int64 10L) ~expectation:(Ir.V_int64 3628800L))
    (*make_test "add_bignat 1+15"
        (expect_script_output
           ~script:
             (pair_wrapper (fun param ->
                  stdlib (fun { add_bignat; uncurry; _ } ->
                      uncurry add_bignat param)))
           ~parameter:
             (Ast.Pair
                ( numlist_to_bignat_ast_value [10L],
                  numlist_to_bignat_ast_value [5L] ))
           ~expectation:(numlist_to_bignat_ir_value [15L]));
      make_test "add_bignat maxint64+1"
        (expect_script_output
           ~script:
             (pair_wrapper (fun param ->
                  stdlib (fun { add_bignat; uncurry; _ } ->
                      uncurry add_bignat param)))
           ~parameter:
             (Ast.Pair
                ( numlist_to_bignat_ast_value [-1L],
                  numlist_to_bignat_ast_value [1L] ))
           ~expectation:
             (V_pair
                {
                  first = V_int64 2L;
                  second = V_pair { first = V_int64 0L; second = V_int64 1L };
                }));
      make_test "add_bignat maxint64+maxint64"
        (expect_script_output
           ~script:
             (pair_wrapper (fun param ->
                  stdlib (fun { add_bignat; uncurry; _ } ->
                      uncurry add_bignat param)))
           ~parameter:
             (Ast.Pair
                ( numlist_to_bignat_ast_value [-1L],
                  numlist_to_bignat_ast_value [-1L] ))
           ~expectation:(numlist_to_bignat_ir_value [-1L; 1L]));
      make_test "add_bignat maxint64^3  maxint64^3"
        (expect_script_output
           ~script:
             (pair_wrapper (fun param ->
                  stdlib (fun { add_bignat; uncurry; _ } ->
                      uncurry add_bignat param)))
           ~parameter:
             (Ast.Pair
                ( numlist_to_bignat_ast_value [-2L; -1L; -1L; -1L],
                  numlist_to_bignat_ast_value [-1L; -1L; -1L] ))
           ~expectation:(numlist_to_bignat_ir_value [-1L; 1L]))*);
  ]

let add_with_carry =
  let expect_bignum_list =
    Alcotest.(
      check
        (testable
           (fun ppf value ->
             Fmt.pf ppf "%s" ([%derive.show: Int64.t list] value))
           [%derive.eq: Int64.t list])) in
  let expect_bignum_list_with_size =
    Alcotest.(
      check
        (testable
           (fun ppf value ->
             Fmt.pf ppf "%s" ([%derive.show: int * Int64.t list] value))
           [%derive.eq: int * Int64.t list])) in
  [
    make_test "Math.add_with_carry 33 11 (no overflow)" (fun description ->
        expect_int64_pair description (44L, 0L) (Math.add_with_carry 33L 11L));
    (* Recall that -1L is treated as max_int64 *)
    make_test "Math.add_with_carry maxint64 0 (no overflow)" (fun description ->
        expect_int64_pair description (-1L, 0L) (Math.add_with_carry (-1L) 0L));
    make_test "Math.add_with_carry maxint64 1 (yes overflow)"
      (fun description ->
        expect_int64_pair description (0L, 1L) (Math.add_with_carry (-1L) 1L));
    make_test "Math.add_with_carry maxint64 maxint64 (yes overflow)"
      (fun description ->
        expect_int64_pair description (-2L, 1L)
          (Math.add_with_carry (-1L) (-1L)));
    make_test
      "Math.triple_add_with_carry maxint64 maxint64 maxint64 (yes overflow)"
      (fun description ->
        expect_int64_pair description (-3L, 2L)
          (Math.triple_add_with_carry (-1L) (-1L) (-1L)));
    (* Should never happen, but might as well test it anyway *)
    make_test "Math.add_bignat empty" (fun description ->
        expect_bignum_list description [] (Math.add_bignat [] []));
    make_test "Math.add_bignat zero zero" (fun description ->
        expect_bignum_list description [0L] (Math.add_bignat [0L] [0L]));
    make_test "Math.add_bignat maxint64 maxint64" (fun description ->
        expect_bignum_list description [-2L; 1L] (Math.add_bignat [-1L] [-1L]));
    make_test "Math.add_bignat maxint64^2 1" (fun description ->
        expect_bignum_list description [0L; 0L; 1L]
          (Math.add_bignat [1L] [-1L; -1L]));
    (* result of this one is:
       0111111111111111111111111111111111111111111111111111111111111111  = -2L
       1111111111111111111111111111111111111111111111111111111111111111  = -1L
       1111111111111111111111111111111111111111111111111111111111111111  = -1L
       1000000000000000000000000000000000000000000000000000000000000000  = 1L
       (little endian)
    *)
    make_test "Math.add_bignat maxint64^3  maxint64^3" (fun description ->
        expect_bignum_list description [-2L; -1L; -1L; 1L]
          (Math.add_bignat [-1L; -1L; -1L] [-1L; -1L; -1L]));
    make_test "Math.add_bignat' zero zero" (fun description ->
        expect_bignum_list_with_size description
          (1, [0L])
          (Math.add_bignat' (1, [0L]) (1, [0L])));
    make_test "Math.add_bignat' 1 maxint64^3" (fun description ->
        expect_bignum_list_with_size description
          (4, [0L; 0L; 0L; 1L])
          (Math.add_bignat' (1, [1L]) (3, [-1L; -1L; -1L])));
    make_test "Math.add_bignat' maxint64^3  maxint64^3" (fun description ->
        expect_bignum_list_with_size description
          (4, [-2L; -1L; -1L; 1L])
          (Math.add_bignat' (3, [-1L; -1L; -1L]) (3, [-1L; -1L; -1L])));
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
