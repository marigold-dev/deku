open BinInt
open Datatypes
open List
open Nat
open Specif
open Co_de_bruijn
open Ligo

(** val compile_usages_aux :
    'a1 -> nat -> usage list -> ('a1, string) Tezos_micheline.Micheline.node list **)

let rec compile_usages_aux nil n = function
| [] -> []
| u :: us0 ->
  (match u with
   | Drop ->
     (Tezos_micheline.Micheline.Prim (nil, "DIG", ((Tezos_micheline.Micheline.Int (nil,
       (Z.of_nat n))) :: []), [])) :: ((Tezos_micheline.Micheline.Seq (nil,
       (compile_usages_aux nil (S n) us0))) :: ((Tezos_micheline.Micheline.Prim (nil,
       "DROP", [], [])) :: []))
   | Keep -> compile_usages_aux nil (S n) us0)

(** val compile_usages :
    'a1 -> usage list -> ('a1, string) Tezos_micheline.Micheline.node list **)

let compile_usages nil us =
  compile_usages_aux nil O us

(** val compile_splitting_aux :
    'a1 -> nat -> splitting -> ('a1, string) Tezos_micheline.Micheline.node list **)

let rec compile_splitting_aux nil n = function
| [] -> []
| s :: ss0 ->
  (match s with
   | Left ->
     (Tezos_micheline.Micheline.Prim (nil, "DIG", ((Tezos_micheline.Micheline.Int (nil,
       (Z.of_nat (add n (length ss0))))) :: []),
       [])) :: (compile_splitting_aux nil (S n) ss0)
   | Right -> compile_splitting_aux nil n ss0
   | Both ->
     app ((Tezos_micheline.Micheline.Prim (nil, "DIG", ((Tezos_micheline.Micheline.Int (nil,
       (Z.of_nat (add n (length ss0))))) :: []), [])) :: ((Tezos_micheline.Micheline.Prim
       (nil, "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "DUG",
       ((Tezos_micheline.Micheline.Int (nil, (Z.of_nat (add (S n) (length ss0))))) :: []),
       [])) :: []))) (compile_splitting_aux nil (S n) ss0))

(** val compile_splitting :
    'a1 -> splitting -> ('a1, string) Tezos_micheline.Micheline.node list **)

let compile_splitting nil ss =
  compile_splitting_aux nil O (rev ss)

(** val comb :
    'a1 -> ('a1, string) Tezos_micheline.Micheline.node list -> ('a1, string)
    Tezos_micheline.Micheline.node **)

let rec comb nil = function
| [] -> Tezos_micheline.Micheline.Prim (nil, "unit", [], [])
| a :: az0 ->
  (match az0 with
   | [] -> a
   | _ :: _ ->
     Tezos_micheline.Micheline.Prim (nil, "pair", (a :: ((comb nil az0) :: [])), []))

(** val coq_PAIR : 'a1 -> nat -> ('a1, string) Tezos_micheline.Micheline.node list **)

let coq_PAIR nil n = match n with
| O -> (Tezos_micheline.Micheline.Prim (nil, "UNIT", [], [])) :: []
| S n0 ->
  (match n0 with
   | O -> []
   | S _ ->
     (Tezos_micheline.Micheline.Prim (nil, "PAIR", ((Tezos_micheline.Micheline.Int (nil,
       (Z.of_nat n))) :: []), [])) :: [])

(** val coq_REV_PAIR : 'a1 -> nat -> ('a1, string) Tezos_micheline.Micheline.node list **)

let coq_REV_PAIR nil n = match n with
| O -> (Tezos_micheline.Micheline.Prim (nil, "UNIT", [], [])) :: []
| S n0 ->
  (match n0 with
   | O -> []
   | S _ ->
     concat
       (repeat ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
         [])) :: ((Tezos_micheline.Micheline.Prim (nil, "PAIR", [], [])) :: []))
         (sub n (S O))))

(** val coq_UNPAIR : 'a1 -> nat -> ('a1, string) Tezos_micheline.Micheline.node list **)

let coq_UNPAIR nil n = match n with
| O -> (Tezos_micheline.Micheline.Prim (nil, "DROP", [], [])) :: []
| S n0 ->
  (match n0 with
   | O -> []
   | S _ ->
     (Tezos_micheline.Micheline.Prim (nil, "UNPAIR", ((Tezos_micheline.Micheline.Int (nil,
       (Z.of_nat n))) :: []), [])) :: [])

(** val coq_GET : 'a1 -> nat -> nat -> ('a1, string) Tezos_micheline.Micheline.node list **)

let coq_GET nil i n =
  let i0 = if PeanoNat.Nat.eqb (S i) n then mul (S (S O)) i else add (mul (S (S O)) i) (S O)
  in
  (Tezos_micheline.Micheline.Prim (nil, "GET", ((Tezos_micheline.Micheline.Int (nil,
  (Z.of_nat i0))) :: []), [])) :: []

(** val coq_UPDATE :
    'a1 -> nat -> nat -> ('a1, string) Tezos_micheline.Micheline.node list **)

let coq_UPDATE nil i n =
  let i0 = if PeanoNat.Nat.eqb (S i) n then mul (S (S O)) i else add (mul (S (S O)) i) (S O)
  in
  (Tezos_micheline.Micheline.Prim (nil, "UPDATE", ((Tezos_micheline.Micheline.Int (nil,
  (Z.of_nat i0))) :: []), [])) :: []

(** val compile_expr :
    'a1 -> ('a2 -> ('a1, 'a2, 'a3) static_args -> ('a1, string)
    Tezos_micheline.Micheline.node list) -> ('a3 -> ('a1, string)
    Tezos_micheline.Micheline.node) -> ('a3 -> ('a1, string) Tezos_micheline.Micheline.node)
    -> ('a1, string) Tezos_micheline.Micheline.node list -> splitting -> ('a1, 'a2, 'a3)
    expr -> ('a1, string) Tezos_micheline.Micheline.node list **)

let compile_expr nil op_code lit_type lit_value =
  let rec compile_expr0 env outer = function
  | E_var _ -> compile_splitting nil outer
  | E_let_in (_, inner, e1, e2) ->
    let (env1, env2) = split inner env in
    let (outer0, inner0) = assoc_splitting outer inner in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer0 e1))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env2 inner0 (filter_keeps (right_usages outer0)) e2))) :: [])
  | E_tuple (_, args0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer args0))) :: ((Tezos_micheline.Micheline.Seq (nil,
      (coq_REV_PAIR nil (args_length args0)))) :: [])
  | E_let_tuple (_, inner, e1, e2) ->
    let (env1, env2) = split inner env in
    let (outer0, inner0) = assoc_splitting outer inner in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer0 e1))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (coq_UNPAIR nil (binds_length e2)))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env2 inner0 (filter_keeps (right_usages outer0)) e2))) :: []))
  | E_proj (_, e0, i, n) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Seq (nil,
      (coq_GET nil i n))) :: [])
  | E_update (_, args0, i, n) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer args0))) :: ((Tezos_micheline.Micheline.Seq (nil,
      (coq_UPDATE nil i n))) :: [])
  | E_app (_, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
      [])) :: ((Tezos_micheline.Micheline.Prim (nil, "EXEC", [], [])) :: []))
  | E_lam (_, e0, b) ->
    let a =
      let Binds (_, l0, _) = e0 in
      (match l0 with
       | [] -> Tezos_micheline.Micheline.Prim (nil, "unit", [], [])
       | a :: l1 ->
         (match l1 with
          | [] -> a
          | _ :: _ -> Tezos_micheline.Micheline.Prim (nil, "unit", [], [])))
    in
    (match env with
     | [] ->
       (Tezos_micheline.Micheline.Seq (nil,
         (compile_splitting nil outer))) :: ((Tezos_micheline.Micheline.Prim (nil, "LAMBDA",
         (a :: (b :: ((Tezos_micheline.Micheline.Seq (nil,
         (compile_binds0 [] [] [] e0))) :: []))), [])) :: [])
     | _ :: _ ->
       let body =
         let Binds (l, l0, e1) = e0 in
         (match l with
          | [] ->
            (Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim (nil,
              "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR", [],
              [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
              [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
              [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
              (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim (nil,
              "DIG", ((Tezos_micheline.Micheline.Int (nil, (Z.of_nat (length env)))) :: []),
              [])) :: ((Tezos_micheline.Micheline.Seq (nil,
              (compile_binds0 env (repeat Left (length env)) (repeat Keep (length env)) e0))) :: [])))
          | u :: l1 ->
            (match u with
             | Drop ->
               (match l1 with
                | [] ->
                  (match l0 with
                   | [] ->
                     (Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim
                       (nil, "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil,
                       "CDR", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
                       [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                       [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
                       (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim
                       (nil, "DIG", ((Tezos_micheline.Micheline.Int (nil,
                       (Z.of_nat (length env)))) :: []),
                       [])) :: ((Tezos_micheline.Micheline.Seq (nil,
                       (compile_binds0 env (repeat Left (length env))
                         (repeat Keep (length env)) e0))) :: [])))
                   | _ :: l2 ->
                     (match l2 with
                      | [] ->
                        (Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                          [])) :: ((Tezos_micheline.Micheline.Seq (nil,
                          (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Seq
                          (nil, (compile_expr0 env (repeat Left (length env)) e1))) :: []))
                      | _ :: _ ->
                        (Tezos_micheline.Micheline.Seq (nil,
                          ((Tezos_micheline.Micheline.Prim (nil, "DUP", [],
                          [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR", [],
                          [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
                          [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                          [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
                          (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim
                          (nil, "DIG", ((Tezos_micheline.Micheline.Int (nil,
                          (Z.of_nat (length env)))) :: []),
                          [])) :: ((Tezos_micheline.Micheline.Seq (nil,
                          (compile_binds0 env (repeat Left (length env))
                            (repeat Keep (length env)) e0))) :: [])))))
                | _ :: _ ->
                  (Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim
                    (nil, "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR",
                    [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
                    [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                    [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
                    (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim
                    (nil, "DIG", ((Tezos_micheline.Micheline.Int (nil,
                    (Z.of_nat (length env)))) :: []),
                    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
                    (compile_binds0 env (repeat Left (length env))
                      (repeat Keep (length env)) e0))) :: []))))
             | Keep ->
               (Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim (nil,
                 "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR", [],
                 [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
                 [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                 [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
                 (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim (nil,
                 "DIG", ((Tezos_micheline.Micheline.Int (nil,
                 (Z.of_nat (length env)))) :: []), [])) :: ((Tezos_micheline.Micheline.Seq
                 (nil,
                 (compile_binds0 env (repeat Left (length env)) (repeat Keep (length env))
                   e0))) :: [])))))
       in
       (Tezos_micheline.Micheline.Seq (nil,
       (compile_splitting nil outer))) :: ((Tezos_micheline.Micheline.Seq (nil,
       (coq_PAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim (nil, "LAMBDA",
       ((Tezos_micheline.Micheline.Prim (nil, "pair", ((comb nil env) :: (a :: [])),
       [])) :: (b :: ((Tezos_micheline.Micheline.Seq (nil, body)) :: []))),
       [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
       [])) :: ((Tezos_micheline.Micheline.Prim (nil, "APPLY", [], [])) :: [])))))
  | E_operator (_, op, sargs, args0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer args0))) :: ((Tezos_micheline.Micheline.Seq (nil,
      (op_code op sargs))) :: [])
  | E_literal (_, lit) ->
    (Tezos_micheline.Micheline.Prim (nil, "PUSH",
      ((lit_type lit) :: ((lit_value lit) :: [])), [])) :: []
  | E_pair (_, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "PAIR", [],
      [])) :: [])
  | E_car (_, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
      [])) :: [])
  | E_cdr (_, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR", [],
      [])) :: [])
  | E_unit _ ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_splitting nil outer))) :: ((Tezos_micheline.Micheline.Prim (nil, "UNIT", [],
      [])) :: [])
  | E_left (_, b, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "LEFT",
      (b :: []), [])) :: [])
  | E_right (_, a, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "RIGHT",
      (a :: []), [])) :: [])
  | E_if_left (_, e0) ->
    let (p, e3) = compile_cond0 env outer e0 in
    let (e1, e2) = p in
    (Tezos_micheline.Micheline.Seq (nil, e1)) :: ((Tezos_micheline.Micheline.Prim (nil,
    "IF_LEFT", ((Tezos_micheline.Micheline.Seq (nil, e2)) :: ((Tezos_micheline.Micheline.Seq
    (nil, e3)) :: [])), [])) :: [])
  | E_if_bool (_, e0) ->
    let (p, e3) = compile_cond0 env outer e0 in
    let (e1, e2) = p in
    (Tezos_micheline.Micheline.Seq (nil, e1)) :: ((Tezos_micheline.Micheline.Prim (nil,
    "IF", ((Tezos_micheline.Micheline.Seq (nil, e2)) :: ((Tezos_micheline.Micheline.Seq
    (nil, e3)) :: [])), [])) :: [])
  | E_if_none (_, e0) ->
    let (p, e3) = compile_cond0 env outer e0 in
    let (e1, e2) = p in
    (Tezos_micheline.Micheline.Seq (nil, e1)) :: ((Tezos_micheline.Micheline.Prim (nil,
    "IF_NONE", ((Tezos_micheline.Micheline.Seq (nil, e2)) :: ((Tezos_micheline.Micheline.Seq
    (nil, e3)) :: [])), [])) :: [])
  | E_if_cons (_, e0) ->
    let (p, e3) = compile_cond0 env outer e0 in
    let (e1, e2) = p in
    (Tezos_micheline.Micheline.Seq (nil, e1)) :: ((Tezos_micheline.Micheline.Prim (nil,
    "IF_CONS", ((Tezos_micheline.Micheline.Seq (nil, e2)) :: ((Tezos_micheline.Micheline.Seq
    (nil, e3)) :: [])), [])) :: [])
  | E_iter (_, inner, e1, e2) ->
    let (env1, env2) = split inner env in
    let inner0 = flip_splitting inner in
    let (outer0, inner1) = assoc_splitting outer inner0 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 outer0 e2))) :: ((Tezos_micheline.Micheline.Prim (nil, "ITER",
    ((Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env1 (keep_rights (left_usages inner1))
      (filter_keeps (right_usages outer0)) e1))) :: ((Tezos_micheline.Micheline.Prim (nil,
    "DROP", [], [])) :: [])))) :: []), [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (right_usages inner1)))) :: ((Tezos_micheline.Micheline.Prim (nil,
    "UNIT", [], [])) :: [])))
  | E_map (_, inner, e1, e2) ->
    let (env1, env2) = split inner env in
    let inner0 = flip_splitting inner in
    let (outer0, inner1) = assoc_splitting outer inner0 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 outer0 e2))) :: ((Tezos_micheline.Micheline.Prim (nil, "MAP",
    ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env1 (keep_rights (left_usages inner1))
      (filter_keeps (right_usages outer0)) e1))) :: []),
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (Keep :: (right_usages inner1))))) :: []))
  | E_loop_left (_, inner, e1, b, e2) ->
    let (env1, env2) = split inner env in
    let inner0 = flip_splitting inner in
    let (outer0, inner1) = assoc_splitting outer inner0 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 outer0 e2))) :: ((Tezos_micheline.Micheline.Prim (nil, "LEFT",
    (b :: []), [])) :: ((Tezos_micheline.Micheline.Prim (nil, "LOOP_LEFT",
    ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env1 (keep_rights (left_usages inner1))
      (filter_keeps (right_usages outer0)) e1))) :: []),
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (Keep :: (right_usages inner1))))) :: [])))
  | E_fold (_, inner1, e1, inner2, e2, e3) ->
    let (env1, env') = split inner1 env in
    let (env2, env3) = split inner2 env' in
    let (outer0, inner3) = assoc_splitting outer inner1 in
    let (inner4, inner5) = assoc_splitting inner3 inner2 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer0 e1))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 (Right :: inner4) e2))) :: ((Tezos_micheline.Micheline.Prim (nil,
    "ITER", ((Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim (nil,
    "SWAP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "PAIR", [],
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env3 (keep_rights (left_usages inner5))
      (filter_keeps (right_usages inner4)) e3))) :: []))))) :: []),
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (Keep :: (right_usages inner5))))) :: [])))
  | E_fold_right (_, elem, inner1, e1, inner2, e2, e3) ->
    let (env1, env') = split inner1 env in
    let (env2, env3) = split inner2 env' in
    let (outer0, inner3) = assoc_splitting outer inner1 in
    let (inner4, inner5) = assoc_splitting inner3 inner2 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer0 e1))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 (Right :: inner4) e2))) :: ((Tezos_micheline.Micheline.Seq (nil,
    ((Tezos_micheline.Micheline.Prim (nil, "NIL", (elem :: []),
    [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
    [])) :: ((Tezos_micheline.Micheline.Prim (nil, "ITER", ((Tezos_micheline.Micheline.Seq
    (nil, ((Tezos_micheline.Micheline.Prim (nil, "CONS", [], [])) :: []))) :: []),
    [])) :: []))))) :: ((Tezos_micheline.Micheline.Prim (nil, "ITER",
    ((Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim (nil, "PAIR", [],
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env3 (keep_rights (left_usages inner5))
      (filter_keeps (right_usages inner4)) e3))) :: [])))) :: []),
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (Keep :: (right_usages inner5))))) :: []))))
  | E_failwith (x, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (x, "FAILWITH", [],
      [])) :: [])
  | E_raw_michelson (_, a, b, code) ->
    (Tezos_micheline.Micheline.Prim (nil, "PUSH", ((Tezos_micheline.Micheline.Prim (nil,
      "lambda", (a :: (b :: [])), [])) :: ((Tezos_micheline.Micheline.Seq (nil,
      code)) :: [])), [])) :: []
  and compile_args0 env outer = function
  | Args_nil -> []
  | Args_cons (inner, e0, args0) ->
    let (env1, env2) = split inner env in
    let (outer', inner') = assoc_splitting outer inner in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer' e0))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_args0 env2 (Right :: inner') args0))) :: [])
  and compile_binds0 env outer proj = function
  | Binds (us, az, e0) ->
    let env' = app (select us az) env in
    let outer' = app (repeat Left (length (select us az))) outer in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (app us proj)))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env' outer' e0))) :: [])
  and compile_cond0 env outer = function
  | Cond (inner1, e1, inner2, b2, b3) ->
    let (env1, env') = split inner1 env in
    let (env2, env3) = split inner2 env' in
    let (outer', inner1') = assoc_splitting outer inner1 in
    let (outerR, innerR) = assoc_splitting inner1' inner2 in
    let (outerL, innerL) = assoc_splitting inner1' (flip_splitting inner2) in
    (((compile_expr0 env1 outer' e1),
    (compile_binds0 env2 innerL (right_usages outerL) b2)),
    (compile_binds0 env3 innerR (right_usages outerR) b3))
  in compile_expr0

(** val compile_args :
    'a1 -> ('a2 -> ('a1, 'a2, 'a3) static_args -> ('a1, string)
    Tezos_micheline.Micheline.node list) -> ('a3 -> ('a1, string)
    Tezos_micheline.Micheline.node) -> ('a3 -> ('a1, string) Tezos_micheline.Micheline.node)
    -> ('a1, string) Tezos_micheline.Micheline.node list -> splitting -> ('a1, 'a2, 'a3)
    args -> ('a1, string) Tezos_micheline.Micheline.node list **)

let compile_args nil op_code lit_type lit_value =
  let rec compile_expr0 env outer = function
  | E_var _ -> compile_splitting nil outer
  | E_let_in (_, inner, e1, e2) ->
    let (env1, env2) = split inner env in
    let (outer0, inner0) = assoc_splitting outer inner in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer0 e1))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env2 inner0 (filter_keeps (right_usages outer0)) e2))) :: [])
  | E_tuple (_, args0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer args0))) :: ((Tezos_micheline.Micheline.Seq (nil,
      (coq_REV_PAIR nil (args_length args0)))) :: [])
  | E_let_tuple (_, inner, e1, e2) ->
    let (env1, env2) = split inner env in
    let (outer0, inner0) = assoc_splitting outer inner in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer0 e1))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (coq_UNPAIR nil (binds_length e2)))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env2 inner0 (filter_keeps (right_usages outer0)) e2))) :: []))
  | E_proj (_, e0, i, n) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Seq (nil,
      (coq_GET nil i n))) :: [])
  | E_update (_, args0, i, n) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer args0))) :: ((Tezos_micheline.Micheline.Seq (nil,
      (coq_UPDATE nil i n))) :: [])
  | E_app (_, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
      [])) :: ((Tezos_micheline.Micheline.Prim (nil, "EXEC", [], [])) :: []))
  | E_lam (_, e0, b) ->
    let a =
      let Binds (_, l0, _) = e0 in
      (match l0 with
       | [] -> Tezos_micheline.Micheline.Prim (nil, "unit", [], [])
       | a :: l1 ->
         (match l1 with
          | [] -> a
          | _ :: _ -> Tezos_micheline.Micheline.Prim (nil, "unit", [], [])))
    in
    (match env with
     | [] ->
       (Tezos_micheline.Micheline.Seq (nil,
         (compile_splitting nil outer))) :: ((Tezos_micheline.Micheline.Prim (nil, "LAMBDA",
         (a :: (b :: ((Tezos_micheline.Micheline.Seq (nil,
         (compile_binds0 [] [] [] e0))) :: []))), [])) :: [])
     | _ :: _ ->
       let body =
         let Binds (l, l0, e1) = e0 in
         (match l with
          | [] ->
            (Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim (nil,
              "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR", [],
              [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
              [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
              [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
              (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim (nil,
              "DIG", ((Tezos_micheline.Micheline.Int (nil, (Z.of_nat (length env)))) :: []),
              [])) :: ((Tezos_micheline.Micheline.Seq (nil,
              (compile_binds0 env (repeat Left (length env)) (repeat Keep (length env)) e0))) :: [])))
          | u :: l1 ->
            (match u with
             | Drop ->
               (match l1 with
                | [] ->
                  (match l0 with
                   | [] ->
                     (Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim
                       (nil, "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil,
                       "CDR", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
                       [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                       [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
                       (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim
                       (nil, "DIG", ((Tezos_micheline.Micheline.Int (nil,
                       (Z.of_nat (length env)))) :: []),
                       [])) :: ((Tezos_micheline.Micheline.Seq (nil,
                       (compile_binds0 env (repeat Left (length env))
                         (repeat Keep (length env)) e0))) :: [])))
                   | _ :: l2 ->
                     (match l2 with
                      | [] ->
                        (Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                          [])) :: ((Tezos_micheline.Micheline.Seq (nil,
                          (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Seq
                          (nil, (compile_expr0 env (repeat Left (length env)) e1))) :: []))
                      | _ :: _ ->
                        (Tezos_micheline.Micheline.Seq (nil,
                          ((Tezos_micheline.Micheline.Prim (nil, "DUP", [],
                          [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR", [],
                          [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
                          [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                          [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
                          (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim
                          (nil, "DIG", ((Tezos_micheline.Micheline.Int (nil,
                          (Z.of_nat (length env)))) :: []),
                          [])) :: ((Tezos_micheline.Micheline.Seq (nil,
                          (compile_binds0 env (repeat Left (length env))
                            (repeat Keep (length env)) e0))) :: [])))))
                | _ :: _ ->
                  (Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim
                    (nil, "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR",
                    [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
                    [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                    [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
                    (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim
                    (nil, "DIG", ((Tezos_micheline.Micheline.Int (nil,
                    (Z.of_nat (length env)))) :: []),
                    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
                    (compile_binds0 env (repeat Left (length env))
                      (repeat Keep (length env)) e0))) :: []))))
             | Keep ->
               (Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim (nil,
                 "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR", [],
                 [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
                 [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                 [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
                 (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim (nil,
                 "DIG", ((Tezos_micheline.Micheline.Int (nil,
                 (Z.of_nat (length env)))) :: []), [])) :: ((Tezos_micheline.Micheline.Seq
                 (nil,
                 (compile_binds0 env (repeat Left (length env)) (repeat Keep (length env))
                   e0))) :: [])))))
       in
       (Tezos_micheline.Micheline.Seq (nil,
       (compile_splitting nil outer))) :: ((Tezos_micheline.Micheline.Seq (nil,
       (coq_PAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim (nil, "LAMBDA",
       ((Tezos_micheline.Micheline.Prim (nil, "pair", ((comb nil env) :: (a :: [])),
       [])) :: (b :: ((Tezos_micheline.Micheline.Seq (nil, body)) :: []))),
       [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
       [])) :: ((Tezos_micheline.Micheline.Prim (nil, "APPLY", [], [])) :: [])))))
  | E_operator (_, op, sargs, args0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer args0))) :: ((Tezos_micheline.Micheline.Seq (nil,
      (op_code op sargs))) :: [])
  | E_literal (_, lit) ->
    (Tezos_micheline.Micheline.Prim (nil, "PUSH",
      ((lit_type lit) :: ((lit_value lit) :: [])), [])) :: []
  | E_pair (_, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "PAIR", [],
      [])) :: [])
  | E_car (_, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
      [])) :: [])
  | E_cdr (_, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR", [],
      [])) :: [])
  | E_unit _ ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_splitting nil outer))) :: ((Tezos_micheline.Micheline.Prim (nil, "UNIT", [],
      [])) :: [])
  | E_left (_, b, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "LEFT",
      (b :: []), [])) :: [])
  | E_right (_, a, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "RIGHT",
      (a :: []), [])) :: [])
  | E_if_left (_, e0) ->
    let (p, e3) = compile_cond0 env outer e0 in
    let (e1, e2) = p in
    (Tezos_micheline.Micheline.Seq (nil, e1)) :: ((Tezos_micheline.Micheline.Prim (nil,
    "IF_LEFT", ((Tezos_micheline.Micheline.Seq (nil, e2)) :: ((Tezos_micheline.Micheline.Seq
    (nil, e3)) :: [])), [])) :: [])
  | E_if_bool (_, e0) ->
    let (p, e3) = compile_cond0 env outer e0 in
    let (e1, e2) = p in
    (Tezos_micheline.Micheline.Seq (nil, e1)) :: ((Tezos_micheline.Micheline.Prim (nil,
    "IF", ((Tezos_micheline.Micheline.Seq (nil, e2)) :: ((Tezos_micheline.Micheline.Seq
    (nil, e3)) :: [])), [])) :: [])
  | E_if_none (_, e0) ->
    let (p, e3) = compile_cond0 env outer e0 in
    let (e1, e2) = p in
    (Tezos_micheline.Micheline.Seq (nil, e1)) :: ((Tezos_micheline.Micheline.Prim (nil,
    "IF_NONE", ((Tezos_micheline.Micheline.Seq (nil, e2)) :: ((Tezos_micheline.Micheline.Seq
    (nil, e3)) :: [])), [])) :: [])
  | E_if_cons (_, e0) ->
    let (p, e3) = compile_cond0 env outer e0 in
    let (e1, e2) = p in
    (Tezos_micheline.Micheline.Seq (nil, e1)) :: ((Tezos_micheline.Micheline.Prim (nil,
    "IF_CONS", ((Tezos_micheline.Micheline.Seq (nil, e2)) :: ((Tezos_micheline.Micheline.Seq
    (nil, e3)) :: [])), [])) :: [])
  | E_iter (_, inner, e1, e2) ->
    let (env1, env2) = split inner env in
    let inner0 = flip_splitting inner in
    let (outer0, inner1) = assoc_splitting outer inner0 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 outer0 e2))) :: ((Tezos_micheline.Micheline.Prim (nil, "ITER",
    ((Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env1 (keep_rights (left_usages inner1))
      (filter_keeps (right_usages outer0)) e1))) :: ((Tezos_micheline.Micheline.Prim (nil,
    "DROP", [], [])) :: [])))) :: []), [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (right_usages inner1)))) :: ((Tezos_micheline.Micheline.Prim (nil,
    "UNIT", [], [])) :: [])))
  | E_map (_, inner, e1, e2) ->
    let (env1, env2) = split inner env in
    let inner0 = flip_splitting inner in
    let (outer0, inner1) = assoc_splitting outer inner0 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 outer0 e2))) :: ((Tezos_micheline.Micheline.Prim (nil, "MAP",
    ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env1 (keep_rights (left_usages inner1))
      (filter_keeps (right_usages outer0)) e1))) :: []),
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (Keep :: (right_usages inner1))))) :: []))
  | E_loop_left (_, inner, e1, b, e2) ->
    let (env1, env2) = split inner env in
    let inner0 = flip_splitting inner in
    let (outer0, inner1) = assoc_splitting outer inner0 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 outer0 e2))) :: ((Tezos_micheline.Micheline.Prim (nil, "LEFT",
    (b :: []), [])) :: ((Tezos_micheline.Micheline.Prim (nil, "LOOP_LEFT",
    ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env1 (keep_rights (left_usages inner1))
      (filter_keeps (right_usages outer0)) e1))) :: []),
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (Keep :: (right_usages inner1))))) :: [])))
  | E_fold (_, inner1, e1, inner2, e2, e3) ->
    let (env1, env') = split inner1 env in
    let (env2, env3) = split inner2 env' in
    let (outer0, inner3) = assoc_splitting outer inner1 in
    let (inner4, inner5) = assoc_splitting inner3 inner2 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer0 e1))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 (Right :: inner4) e2))) :: ((Tezos_micheline.Micheline.Prim (nil,
    "ITER", ((Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim (nil,
    "SWAP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "PAIR", [],
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env3 (keep_rights (left_usages inner5))
      (filter_keeps (right_usages inner4)) e3))) :: []))))) :: []),
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (Keep :: (right_usages inner5))))) :: [])))
  | E_fold_right (_, elem, inner1, e1, inner2, e2, e3) ->
    let (env1, env') = split inner1 env in
    let (env2, env3) = split inner2 env' in
    let (outer0, inner3) = assoc_splitting outer inner1 in
    let (inner4, inner5) = assoc_splitting inner3 inner2 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer0 e1))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 (Right :: inner4) e2))) :: ((Tezos_micheline.Micheline.Seq (nil,
    ((Tezos_micheline.Micheline.Prim (nil, "NIL", (elem :: []),
    [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
    [])) :: ((Tezos_micheline.Micheline.Prim (nil, "ITER", ((Tezos_micheline.Micheline.Seq
    (nil, ((Tezos_micheline.Micheline.Prim (nil, "CONS", [], [])) :: []))) :: []),
    [])) :: []))))) :: ((Tezos_micheline.Micheline.Prim (nil, "ITER",
    ((Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim (nil, "PAIR", [],
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env3 (keep_rights (left_usages inner5))
      (filter_keeps (right_usages inner4)) e3))) :: [])))) :: []),
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (Keep :: (right_usages inner5))))) :: []))))
  | E_failwith (x, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (x, "FAILWITH", [],
      [])) :: [])
  | E_raw_michelson (_, a, b, code) ->
    (Tezos_micheline.Micheline.Prim (nil, "PUSH", ((Tezos_micheline.Micheline.Prim (nil,
      "lambda", (a :: (b :: [])), [])) :: ((Tezos_micheline.Micheline.Seq (nil,
      code)) :: [])), [])) :: []
  and compile_args0 env outer = function
  | Args_nil -> []
  | Args_cons (inner, e0, args0) ->
    let (env1, env2) = split inner env in
    let (outer', inner') = assoc_splitting outer inner in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer' e0))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_args0 env2 (Right :: inner') args0))) :: [])
  and compile_binds0 env outer proj = function
  | Binds (us, az, e0) ->
    let env' = app (select us az) env in
    let outer' = app (repeat Left (length (select us az))) outer in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (app us proj)))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env' outer' e0))) :: [])
  and compile_cond0 env outer = function
  | Cond (inner1, e1, inner2, b2, b3) ->
    let (env1, env') = split inner1 env in
    let (env2, env3) = split inner2 env' in
    let (outer', inner1') = assoc_splitting outer inner1 in
    let (outerR, innerR) = assoc_splitting inner1' inner2 in
    let (outerL, innerL) = assoc_splitting inner1' (flip_splitting inner2) in
    (((compile_expr0 env1 outer' e1),
    (compile_binds0 env2 innerL (right_usages outerL) b2)),
    (compile_binds0 env3 innerR (right_usages outerR) b3))
  in compile_args0

(** val compile_binds :
    'a1 -> ('a2 -> ('a1, 'a2, 'a3) static_args -> ('a1, string)
    Tezos_micheline.Micheline.node list) -> ('a3 -> ('a1, string)
    Tezos_micheline.Micheline.node) -> ('a3 -> ('a1, string) Tezos_micheline.Micheline.node)
    -> ('a1, string) Tezos_micheline.Micheline.node list -> splitting -> usage list -> ('a1,
    'a2, 'a3) binds -> ('a1, string) Tezos_micheline.Micheline.node list **)

let compile_binds nil op_code lit_type lit_value =
  let rec compile_expr0 env outer = function
  | E_var _ -> compile_splitting nil outer
  | E_let_in (_, inner, e1, e2) ->
    let (env1, env2) = split inner env in
    let (outer0, inner0) = assoc_splitting outer inner in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer0 e1))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env2 inner0 (filter_keeps (right_usages outer0)) e2))) :: [])
  | E_tuple (_, args0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer args0))) :: ((Tezos_micheline.Micheline.Seq (nil,
      (coq_REV_PAIR nil (args_length args0)))) :: [])
  | E_let_tuple (_, inner, e1, e2) ->
    let (env1, env2) = split inner env in
    let (outer0, inner0) = assoc_splitting outer inner in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer0 e1))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (coq_UNPAIR nil (binds_length e2)))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env2 inner0 (filter_keeps (right_usages outer0)) e2))) :: []))
  | E_proj (_, e0, i, n) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Seq (nil,
      (coq_GET nil i n))) :: [])
  | E_update (_, args0, i, n) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer args0))) :: ((Tezos_micheline.Micheline.Seq (nil,
      (coq_UPDATE nil i n))) :: [])
  | E_app (_, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
      [])) :: ((Tezos_micheline.Micheline.Prim (nil, "EXEC", [], [])) :: []))
  | E_lam (_, e0, b) ->
    let a =
      let Binds (_, l0, _) = e0 in
      (match l0 with
       | [] -> Tezos_micheline.Micheline.Prim (nil, "unit", [], [])
       | a :: l1 ->
         (match l1 with
          | [] -> a
          | _ :: _ -> Tezos_micheline.Micheline.Prim (nil, "unit", [], [])))
    in
    (match env with
     | [] ->
       (Tezos_micheline.Micheline.Seq (nil,
         (compile_splitting nil outer))) :: ((Tezos_micheline.Micheline.Prim (nil, "LAMBDA",
         (a :: (b :: ((Tezos_micheline.Micheline.Seq (nil,
         (compile_binds0 [] [] [] e0))) :: []))), [])) :: [])
     | _ :: _ ->
       let body =
         let Binds (l, l0, e1) = e0 in
         (match l with
          | [] ->
            (Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim (nil,
              "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR", [],
              [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
              [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
              [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
              (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim (nil,
              "DIG", ((Tezos_micheline.Micheline.Int (nil, (Z.of_nat (length env)))) :: []),
              [])) :: ((Tezos_micheline.Micheline.Seq (nil,
              (compile_binds0 env (repeat Left (length env)) (repeat Keep (length env)) e0))) :: [])))
          | u :: l1 ->
            (match u with
             | Drop ->
               (match l1 with
                | [] ->
                  (match l0 with
                   | [] ->
                     (Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim
                       (nil, "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil,
                       "CDR", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
                       [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                       [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
                       (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim
                       (nil, "DIG", ((Tezos_micheline.Micheline.Int (nil,
                       (Z.of_nat (length env)))) :: []),
                       [])) :: ((Tezos_micheline.Micheline.Seq (nil,
                       (compile_binds0 env (repeat Left (length env))
                         (repeat Keep (length env)) e0))) :: [])))
                   | _ :: l2 ->
                     (match l2 with
                      | [] ->
                        (Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                          [])) :: ((Tezos_micheline.Micheline.Seq (nil,
                          (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Seq
                          (nil, (compile_expr0 env (repeat Left (length env)) e1))) :: []))
                      | _ :: _ ->
                        (Tezos_micheline.Micheline.Seq (nil,
                          ((Tezos_micheline.Micheline.Prim (nil, "DUP", [],
                          [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR", [],
                          [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
                          [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                          [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
                          (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim
                          (nil, "DIG", ((Tezos_micheline.Micheline.Int (nil,
                          (Z.of_nat (length env)))) :: []),
                          [])) :: ((Tezos_micheline.Micheline.Seq (nil,
                          (compile_binds0 env (repeat Left (length env))
                            (repeat Keep (length env)) e0))) :: [])))))
                | _ :: _ ->
                  (Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim
                    (nil, "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR",
                    [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
                    [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                    [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
                    (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim
                    (nil, "DIG", ((Tezos_micheline.Micheline.Int (nil,
                    (Z.of_nat (length env)))) :: []),
                    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
                    (compile_binds0 env (repeat Left (length env))
                      (repeat Keep (length env)) e0))) :: []))))
             | Keep ->
               (Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim (nil,
                 "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR", [],
                 [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
                 [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                 [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
                 (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim (nil,
                 "DIG", ((Tezos_micheline.Micheline.Int (nil,
                 (Z.of_nat (length env)))) :: []), [])) :: ((Tezos_micheline.Micheline.Seq
                 (nil,
                 (compile_binds0 env (repeat Left (length env)) (repeat Keep (length env))
                   e0))) :: [])))))
       in
       (Tezos_micheline.Micheline.Seq (nil,
       (compile_splitting nil outer))) :: ((Tezos_micheline.Micheline.Seq (nil,
       (coq_PAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim (nil, "LAMBDA",
       ((Tezos_micheline.Micheline.Prim (nil, "pair", ((comb nil env) :: (a :: [])),
       [])) :: (b :: ((Tezos_micheline.Micheline.Seq (nil, body)) :: []))),
       [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
       [])) :: ((Tezos_micheline.Micheline.Prim (nil, "APPLY", [], [])) :: [])))))
  | E_operator (_, op, sargs, args0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer args0))) :: ((Tezos_micheline.Micheline.Seq (nil,
      (op_code op sargs))) :: [])
  | E_literal (_, lit) ->
    (Tezos_micheline.Micheline.Prim (nil, "PUSH",
      ((lit_type lit) :: ((lit_value lit) :: [])), [])) :: []
  | E_pair (_, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "PAIR", [],
      [])) :: [])
  | E_car (_, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
      [])) :: [])
  | E_cdr (_, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR", [],
      [])) :: [])
  | E_unit _ ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_splitting nil outer))) :: ((Tezos_micheline.Micheline.Prim (nil, "UNIT", [],
      [])) :: [])
  | E_left (_, b, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "LEFT",
      (b :: []), [])) :: [])
  | E_right (_, a, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "RIGHT",
      (a :: []), [])) :: [])
  | E_if_left (_, e0) ->
    let (p, e3) = compile_cond0 env outer e0 in
    let (e1, e2) = p in
    (Tezos_micheline.Micheline.Seq (nil, e1)) :: ((Tezos_micheline.Micheline.Prim (nil,
    "IF_LEFT", ((Tezos_micheline.Micheline.Seq (nil, e2)) :: ((Tezos_micheline.Micheline.Seq
    (nil, e3)) :: [])), [])) :: [])
  | E_if_bool (_, e0) ->
    let (p, e3) = compile_cond0 env outer e0 in
    let (e1, e2) = p in
    (Tezos_micheline.Micheline.Seq (nil, e1)) :: ((Tezos_micheline.Micheline.Prim (nil,
    "IF", ((Tezos_micheline.Micheline.Seq (nil, e2)) :: ((Tezos_micheline.Micheline.Seq
    (nil, e3)) :: [])), [])) :: [])
  | E_if_none (_, e0) ->
    let (p, e3) = compile_cond0 env outer e0 in
    let (e1, e2) = p in
    (Tezos_micheline.Micheline.Seq (nil, e1)) :: ((Tezos_micheline.Micheline.Prim (nil,
    "IF_NONE", ((Tezos_micheline.Micheline.Seq (nil, e2)) :: ((Tezos_micheline.Micheline.Seq
    (nil, e3)) :: [])), [])) :: [])
  | E_if_cons (_, e0) ->
    let (p, e3) = compile_cond0 env outer e0 in
    let (e1, e2) = p in
    (Tezos_micheline.Micheline.Seq (nil, e1)) :: ((Tezos_micheline.Micheline.Prim (nil,
    "IF_CONS", ((Tezos_micheline.Micheline.Seq (nil, e2)) :: ((Tezos_micheline.Micheline.Seq
    (nil, e3)) :: [])), [])) :: [])
  | E_iter (_, inner, e1, e2) ->
    let (env1, env2) = split inner env in
    let inner0 = flip_splitting inner in
    let (outer0, inner1) = assoc_splitting outer inner0 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 outer0 e2))) :: ((Tezos_micheline.Micheline.Prim (nil, "ITER",
    ((Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env1 (keep_rights (left_usages inner1))
      (filter_keeps (right_usages outer0)) e1))) :: ((Tezos_micheline.Micheline.Prim (nil,
    "DROP", [], [])) :: [])))) :: []), [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (right_usages inner1)))) :: ((Tezos_micheline.Micheline.Prim (nil,
    "UNIT", [], [])) :: [])))
  | E_map (_, inner, e1, e2) ->
    let (env1, env2) = split inner env in
    let inner0 = flip_splitting inner in
    let (outer0, inner1) = assoc_splitting outer inner0 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 outer0 e2))) :: ((Tezos_micheline.Micheline.Prim (nil, "MAP",
    ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env1 (keep_rights (left_usages inner1))
      (filter_keeps (right_usages outer0)) e1))) :: []),
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (Keep :: (right_usages inner1))))) :: []))
  | E_loop_left (_, inner, e1, b, e2) ->
    let (env1, env2) = split inner env in
    let inner0 = flip_splitting inner in
    let (outer0, inner1) = assoc_splitting outer inner0 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 outer0 e2))) :: ((Tezos_micheline.Micheline.Prim (nil, "LEFT",
    (b :: []), [])) :: ((Tezos_micheline.Micheline.Prim (nil, "LOOP_LEFT",
    ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env1 (keep_rights (left_usages inner1))
      (filter_keeps (right_usages outer0)) e1))) :: []),
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (Keep :: (right_usages inner1))))) :: [])))
  | E_fold (_, inner1, e1, inner2, e2, e3) ->
    let (env1, env') = split inner1 env in
    let (env2, env3) = split inner2 env' in
    let (outer0, inner3) = assoc_splitting outer inner1 in
    let (inner4, inner5) = assoc_splitting inner3 inner2 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer0 e1))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 (Right :: inner4) e2))) :: ((Tezos_micheline.Micheline.Prim (nil,
    "ITER", ((Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim (nil,
    "SWAP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "PAIR", [],
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env3 (keep_rights (left_usages inner5))
      (filter_keeps (right_usages inner4)) e3))) :: []))))) :: []),
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (Keep :: (right_usages inner5))))) :: [])))
  | E_fold_right (_, elem, inner1, e1, inner2, e2, e3) ->
    let (env1, env') = split inner1 env in
    let (env2, env3) = split inner2 env' in
    let (outer0, inner3) = assoc_splitting outer inner1 in
    let (inner4, inner5) = assoc_splitting inner3 inner2 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer0 e1))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 (Right :: inner4) e2))) :: ((Tezos_micheline.Micheline.Seq (nil,
    ((Tezos_micheline.Micheline.Prim (nil, "NIL", (elem :: []),
    [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
    [])) :: ((Tezos_micheline.Micheline.Prim (nil, "ITER", ((Tezos_micheline.Micheline.Seq
    (nil, ((Tezos_micheline.Micheline.Prim (nil, "CONS", [], [])) :: []))) :: []),
    [])) :: []))))) :: ((Tezos_micheline.Micheline.Prim (nil, "ITER",
    ((Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim (nil, "PAIR", [],
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env3 (keep_rights (left_usages inner5))
      (filter_keeps (right_usages inner4)) e3))) :: [])))) :: []),
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (Keep :: (right_usages inner5))))) :: []))))
  | E_failwith (x, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (x, "FAILWITH", [],
      [])) :: [])
  | E_raw_michelson (_, a, b, code) ->
    (Tezos_micheline.Micheline.Prim (nil, "PUSH", ((Tezos_micheline.Micheline.Prim (nil,
      "lambda", (a :: (b :: [])), [])) :: ((Tezos_micheline.Micheline.Seq (nil,
      code)) :: [])), [])) :: []
  and compile_args0 env outer = function
  | Args_nil -> []
  | Args_cons (inner, e0, args0) ->
    let (env1, env2) = split inner env in
    let (outer', inner') = assoc_splitting outer inner in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer' e0))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_args0 env2 (Right :: inner') args0))) :: [])
  and compile_binds0 env outer proj = function
  | Binds (us, az, e0) ->
    let env' = app (select us az) env in
    let outer' = app (repeat Left (length (select us az))) outer in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (app us proj)))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env' outer' e0))) :: [])
  and compile_cond0 env outer = function
  | Cond (inner1, e1, inner2, b2, b3) ->
    let (env1, env') = split inner1 env in
    let (env2, env3) = split inner2 env' in
    let (outer', inner1') = assoc_splitting outer inner1 in
    let (outerR, innerR) = assoc_splitting inner1' inner2 in
    let (outerL, innerL) = assoc_splitting inner1' (flip_splitting inner2) in
    (((compile_expr0 env1 outer' e1),
    (compile_binds0 env2 innerL (right_usages outerL) b2)),
    (compile_binds0 env3 innerR (right_usages outerR) b3))
  in compile_binds0

(** val compile_cond :
    'a1 -> ('a2 -> ('a1, 'a2, 'a3) static_args -> ('a1, string)
    Tezos_micheline.Micheline.node list) -> ('a3 -> ('a1, string)
    Tezos_micheline.Micheline.node) -> ('a3 -> ('a1, string) Tezos_micheline.Micheline.node)
    -> ('a1, string) Tezos_micheline.Micheline.node list -> splitting -> ('a1, 'a2, 'a3)
    cond -> (('a1, string) Tezos_micheline.Micheline.node list * ('a1, string)
    Tezos_micheline.Micheline.node list) * ('a1, string) Tezos_micheline.Micheline.node list **)

let compile_cond nil op_code lit_type lit_value =
  let rec compile_expr0 env outer = function
  | E_var _ -> compile_splitting nil outer
  | E_let_in (_, inner, e1, e2) ->
    let (env1, env2) = split inner env in
    let (outer0, inner0) = assoc_splitting outer inner in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer0 e1))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env2 inner0 (filter_keeps (right_usages outer0)) e2))) :: [])
  | E_tuple (_, args0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer args0))) :: ((Tezos_micheline.Micheline.Seq (nil,
      (coq_REV_PAIR nil (args_length args0)))) :: [])
  | E_let_tuple (_, inner, e1, e2) ->
    let (env1, env2) = split inner env in
    let (outer0, inner0) = assoc_splitting outer inner in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer0 e1))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (coq_UNPAIR nil (binds_length e2)))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env2 inner0 (filter_keeps (right_usages outer0)) e2))) :: []))
  | E_proj (_, e0, i, n) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Seq (nil,
      (coq_GET nil i n))) :: [])
  | E_update (_, args0, i, n) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer args0))) :: ((Tezos_micheline.Micheline.Seq (nil,
      (coq_UPDATE nil i n))) :: [])
  | E_app (_, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
      [])) :: ((Tezos_micheline.Micheline.Prim (nil, "EXEC", [], [])) :: []))
  | E_lam (_, e0, b) ->
    let a =
      let Binds (_, l0, _) = e0 in
      (match l0 with
       | [] -> Tezos_micheline.Micheline.Prim (nil, "unit", [], [])
       | a :: l1 ->
         (match l1 with
          | [] -> a
          | _ :: _ -> Tezos_micheline.Micheline.Prim (nil, "unit", [], [])))
    in
    (match env with
     | [] ->
       (Tezos_micheline.Micheline.Seq (nil,
         (compile_splitting nil outer))) :: ((Tezos_micheline.Micheline.Prim (nil, "LAMBDA",
         (a :: (b :: ((Tezos_micheline.Micheline.Seq (nil,
         (compile_binds0 [] [] [] e0))) :: []))), [])) :: [])
     | _ :: _ ->
       let body =
         let Binds (l, l0, e1) = e0 in
         (match l with
          | [] ->
            (Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim (nil,
              "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR", [],
              [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
              [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
              [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
              (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim (nil,
              "DIG", ((Tezos_micheline.Micheline.Int (nil, (Z.of_nat (length env)))) :: []),
              [])) :: ((Tezos_micheline.Micheline.Seq (nil,
              (compile_binds0 env (repeat Left (length env)) (repeat Keep (length env)) e0))) :: [])))
          | u :: l1 ->
            (match u with
             | Drop ->
               (match l1 with
                | [] ->
                  (match l0 with
                   | [] ->
                     (Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim
                       (nil, "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil,
                       "CDR", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
                       [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                       [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
                       (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim
                       (nil, "DIG", ((Tezos_micheline.Micheline.Int (nil,
                       (Z.of_nat (length env)))) :: []),
                       [])) :: ((Tezos_micheline.Micheline.Seq (nil,
                       (compile_binds0 env (repeat Left (length env))
                         (repeat Keep (length env)) e0))) :: [])))
                   | _ :: l2 ->
                     (match l2 with
                      | [] ->
                        (Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                          [])) :: ((Tezos_micheline.Micheline.Seq (nil,
                          (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Seq
                          (nil, (compile_expr0 env (repeat Left (length env)) e1))) :: []))
                      | _ :: _ ->
                        (Tezos_micheline.Micheline.Seq (nil,
                          ((Tezos_micheline.Micheline.Prim (nil, "DUP", [],
                          [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR", [],
                          [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
                          [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                          [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
                          (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim
                          (nil, "DIG", ((Tezos_micheline.Micheline.Int (nil,
                          (Z.of_nat (length env)))) :: []),
                          [])) :: ((Tezos_micheline.Micheline.Seq (nil,
                          (compile_binds0 env (repeat Left (length env))
                            (repeat Keep (length env)) e0))) :: [])))))
                | _ :: _ ->
                  (Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim
                    (nil, "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR",
                    [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
                    [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                    [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
                    (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim
                    (nil, "DIG", ((Tezos_micheline.Micheline.Int (nil,
                    (Z.of_nat (length env)))) :: []),
                    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
                    (compile_binds0 env (repeat Left (length env))
                      (repeat Keep (length env)) e0))) :: []))))
             | Keep ->
               (Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim (nil,
                 "DUP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR", [],
                 [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
                 [])) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
                 [])) :: [])))))) :: ((Tezos_micheline.Micheline.Seq (nil,
                 (coq_UNPAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim (nil,
                 "DIG", ((Tezos_micheline.Micheline.Int (nil,
                 (Z.of_nat (length env)))) :: []), [])) :: ((Tezos_micheline.Micheline.Seq
                 (nil,
                 (compile_binds0 env (repeat Left (length env)) (repeat Keep (length env))
                   e0))) :: [])))))
       in
       (Tezos_micheline.Micheline.Seq (nil,
       (compile_splitting nil outer))) :: ((Tezos_micheline.Micheline.Seq (nil,
       (coq_PAIR nil (length env)))) :: ((Tezos_micheline.Micheline.Prim (nil, "LAMBDA",
       ((Tezos_micheline.Micheline.Prim (nil, "pair", ((comb nil env) :: (a :: [])),
       [])) :: (b :: ((Tezos_micheline.Micheline.Seq (nil, body)) :: []))),
       [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
       [])) :: ((Tezos_micheline.Micheline.Prim (nil, "APPLY", [], [])) :: [])))))
  | E_operator (_, op, sargs, args0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer args0))) :: ((Tezos_micheline.Micheline.Seq (nil,
      (op_code op sargs))) :: [])
  | E_literal (_, lit) ->
    (Tezos_micheline.Micheline.Prim (nil, "PUSH",
      ((lit_type lit) :: ((lit_value lit) :: [])), [])) :: []
  | E_pair (_, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_args0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "PAIR", [],
      [])) :: [])
  | E_car (_, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "CAR", [],
      [])) :: [])
  | E_cdr (_, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "CDR", [],
      [])) :: [])
  | E_unit _ ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_splitting nil outer))) :: ((Tezos_micheline.Micheline.Prim (nil, "UNIT", [],
      [])) :: [])
  | E_left (_, b, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "LEFT",
      (b :: []), [])) :: [])
  | E_right (_, a, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (nil, "RIGHT",
      (a :: []), [])) :: [])
  | E_if_left (_, e0) ->
    let (p, e3) = compile_cond0 env outer e0 in
    let (e1, e2) = p in
    (Tezos_micheline.Micheline.Seq (nil, e1)) :: ((Tezos_micheline.Micheline.Prim (nil,
    "IF_LEFT", ((Tezos_micheline.Micheline.Seq (nil, e2)) :: ((Tezos_micheline.Micheline.Seq
    (nil, e3)) :: [])), [])) :: [])
  | E_if_bool (_, e0) ->
    let (p, e3) = compile_cond0 env outer e0 in
    let (e1, e2) = p in
    (Tezos_micheline.Micheline.Seq (nil, e1)) :: ((Tezos_micheline.Micheline.Prim (nil,
    "IF", ((Tezos_micheline.Micheline.Seq (nil, e2)) :: ((Tezos_micheline.Micheline.Seq
    (nil, e3)) :: [])), [])) :: [])
  | E_if_none (_, e0) ->
    let (p, e3) = compile_cond0 env outer e0 in
    let (e1, e2) = p in
    (Tezos_micheline.Micheline.Seq (nil, e1)) :: ((Tezos_micheline.Micheline.Prim (nil,
    "IF_NONE", ((Tezos_micheline.Micheline.Seq (nil, e2)) :: ((Tezos_micheline.Micheline.Seq
    (nil, e3)) :: [])), [])) :: [])
  | E_if_cons (_, e0) ->
    let (p, e3) = compile_cond0 env outer e0 in
    let (e1, e2) = p in
    (Tezos_micheline.Micheline.Seq (nil, e1)) :: ((Tezos_micheline.Micheline.Prim (nil,
    "IF_CONS", ((Tezos_micheline.Micheline.Seq (nil, e2)) :: ((Tezos_micheline.Micheline.Seq
    (nil, e3)) :: [])), [])) :: [])
  | E_iter (_, inner, e1, e2) ->
    let (env1, env2) = split inner env in
    let inner0 = flip_splitting inner in
    let (outer0, inner1) = assoc_splitting outer inner0 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 outer0 e2))) :: ((Tezos_micheline.Micheline.Prim (nil, "ITER",
    ((Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env1 (keep_rights (left_usages inner1))
      (filter_keeps (right_usages outer0)) e1))) :: ((Tezos_micheline.Micheline.Prim (nil,
    "DROP", [], [])) :: [])))) :: []), [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (right_usages inner1)))) :: ((Tezos_micheline.Micheline.Prim (nil,
    "UNIT", [], [])) :: [])))
  | E_map (_, inner, e1, e2) ->
    let (env1, env2) = split inner env in
    let inner0 = flip_splitting inner in
    let (outer0, inner1) = assoc_splitting outer inner0 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 outer0 e2))) :: ((Tezos_micheline.Micheline.Prim (nil, "MAP",
    ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env1 (keep_rights (left_usages inner1))
      (filter_keeps (right_usages outer0)) e1))) :: []),
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (Keep :: (right_usages inner1))))) :: []))
  | E_loop_left (_, inner, e1, b, e2) ->
    let (env1, env2) = split inner env in
    let inner0 = flip_splitting inner in
    let (outer0, inner1) = assoc_splitting outer inner0 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 outer0 e2))) :: ((Tezos_micheline.Micheline.Prim (nil, "LEFT",
    (b :: []), [])) :: ((Tezos_micheline.Micheline.Prim (nil, "LOOP_LEFT",
    ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env1 (keep_rights (left_usages inner1))
      (filter_keeps (right_usages outer0)) e1))) :: []),
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (Keep :: (right_usages inner1))))) :: [])))
  | E_fold (_, inner1, e1, inner2, e2, e3) ->
    let (env1, env') = split inner1 env in
    let (env2, env3) = split inner2 env' in
    let (outer0, inner3) = assoc_splitting outer inner1 in
    let (inner4, inner5) = assoc_splitting inner3 inner2 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer0 e1))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 (Right :: inner4) e2))) :: ((Tezos_micheline.Micheline.Prim (nil,
    "ITER", ((Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim (nil,
    "SWAP", [], [])) :: ((Tezos_micheline.Micheline.Prim (nil, "PAIR", [],
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env3 (keep_rights (left_usages inner5))
      (filter_keeps (right_usages inner4)) e3))) :: []))))) :: []),
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (Keep :: (right_usages inner5))))) :: [])))
  | E_fold_right (_, elem, inner1, e1, inner2, e2, e3) ->
    let (env1, env') = split inner1 env in
    let (env2, env3) = split inner2 env' in
    let (outer0, inner3) = assoc_splitting outer inner1 in
    let (inner4, inner5) = assoc_splitting inner3 inner2 in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer0 e1))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env2 (Right :: inner4) e2))) :: ((Tezos_micheline.Micheline.Seq (nil,
    ((Tezos_micheline.Micheline.Prim (nil, "NIL", (elem :: []),
    [])) :: ((Tezos_micheline.Micheline.Prim (nil, "SWAP", [],
    [])) :: ((Tezos_micheline.Micheline.Prim (nil, "ITER", ((Tezos_micheline.Micheline.Seq
    (nil, ((Tezos_micheline.Micheline.Prim (nil, "CONS", [], [])) :: []))) :: []),
    [])) :: []))))) :: ((Tezos_micheline.Micheline.Prim (nil, "ITER",
    ((Tezos_micheline.Micheline.Seq (nil, ((Tezos_micheline.Micheline.Prim (nil, "PAIR", [],
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_binds0 env3 (keep_rights (left_usages inner5))
      (filter_keeps (right_usages inner4)) e3))) :: [])))) :: []),
    [])) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (Keep :: (right_usages inner5))))) :: []))))
  | E_failwith (x, e0) ->
    (Tezos_micheline.Micheline.Seq (nil,
      (compile_expr0 env outer e0))) :: ((Tezos_micheline.Micheline.Prim (x, "FAILWITH", [],
      [])) :: [])
  | E_raw_michelson (_, a, b, code) ->
    (Tezos_micheline.Micheline.Prim (nil, "PUSH", ((Tezos_micheline.Micheline.Prim (nil,
      "lambda", (a :: (b :: [])), [])) :: ((Tezos_micheline.Micheline.Seq (nil,
      code)) :: [])), [])) :: []
  and compile_args0 env outer = function
  | Args_nil -> []
  | Args_cons (inner, e0, args0) ->
    let (env1, env2) = split inner env in
    let (outer', inner') = assoc_splitting outer inner in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env1 outer' e0))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_args0 env2 (Right :: inner') args0))) :: [])
  and compile_binds0 env outer proj = function
  | Binds (us, az, e0) ->
    let env' = app (select us az) env in
    let outer' = app (repeat Left (length (select us az))) outer in
    (Tezos_micheline.Micheline.Seq (nil,
    (compile_usages nil (app us proj)))) :: ((Tezos_micheline.Micheline.Seq (nil,
    (compile_expr0 env' outer' e0))) :: [])
  and compile_cond0 env outer = function
  | Cond (inner1, e1, inner2, b2, b3) ->
    let (env1, env') = split inner1 env in
    let (env2, env3) = split inner2 env' in
    let (outer', inner1') = assoc_splitting outer inner1 in
    let (outerR, innerR) = assoc_splitting inner1' inner2 in
    let (outerL, innerL) = assoc_splitting inner1' (flip_splitting inner2) in
    (((compile_expr0 env1 outer' e1),
    (compile_binds0 env2 innerL (right_usages outerL) b2)),
    (compile_binds0 env3 innerR (right_usages outerR) b3))
  in compile_cond0

(** val destruct_last : 'a1 list -> ('a1, 'a1 list) sigT option **)

let rec destruct_last = function
| [] -> None
| y :: l0 ->
  Some
    (match destruct_last l0 with
     | Some s -> let Coq_existT (x, s0) = s in Coq_existT (x, (y :: s0))
     | None -> Coq_existT (y, []))
