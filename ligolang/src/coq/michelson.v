Local Set Warnings "-implicit-core-hint-db".
Set Implicit Arguments.
From Coq Require Import String List ZArith.
Import ListNotations.

From ligo_coq Require Import types micheline.

Section instr.
Context {A : Set}.
Context {nil : A}.
Context {constant : list (node A string) -> node A string -> Set}.

Local Open Scope string_scope.

Local Generalizable Variable l n.

(* ugh... *)
Inductive comb_ty : node A string -> list (node A string) -> Prop :=
| Comb_two {a b} :
    `{comb_ty (Prim l "pair" [a; b] n) [a; b]}
| Comb_cons {c ts t} :
    `{comb_ty c ts ->
      comb_ty (Prim l "pair" [t; c] n) (t :: ts)}.

Inductive instr_typed : node A string -> list (node A string) -> list (node A string) -> Prop :=
| Typed_seq {p s1 s2} :
    `{prog_typed p s1 s2 ->
      instr_typed (Seq l p) s1 s2}
| Typed_dip {p a s1 s2} :
    `{prog_typed p s1 s2 ->
      instr_typed (Prim l "DIP" p n) (a :: s1) (a :: s2)}
| Typed_dig {k a s1 s2} :
    `{length s1 = k ->
      instr_typed (Prim l1 "DIG" [Int l2 (Z.of_nat k)] n1) (s1 ++ a :: s2) (a :: s1 ++ s2)}
| Typed_dug {k a s1 s2} :
    `{length s1 = k ->
      instr_typed (Prim l1 "DUG" [Int l2 (Z.of_nat k)] n1) (a :: s1 ++ s2) (s1 ++ a :: s2)}
| Typed_dup {a s} :
    `{instr_typed (Prim l "DUP" [] n) (a :: s) (a :: a :: s)}
| Typed_drop {a s} :
    `{instr_typed (Prim l "DROP" [] n) (a :: s) s}
| Typed_swap {a b s} :
    `{instr_typed (Prim l "SWAP" [] n) (a :: b :: s) (b :: a :: s)}
| Typed_exec {a b s} :
    `{instr_typed (Prim l1 "EXEC" [] n1) (a :: Prim l2 "lambda" [a; b] n2 :: s) (b :: s)}
| Typed_apply {a b c s} :
    `{instr_typed (Prim l1 "APPLY" [] n1) (c :: Prim l2 "lambda" [Prim l3 "pair" [c; a] n3; b] n2 :: s) (Prim l4 "lambda" [a; b] n4 :: s)}
| Typed_lambda {a b p s} :
    `{prog_typed p [a] [b] ->
      instr_typed (Prim l1 "LAMBDA" [a; b; Seq l2 p] n1) s (Prim l3 "lambda" [a; b] n3 :: s)}
| Typed_push_lambda {a b p s} :
    `{prog_typed p [a] [b] ->
      instr_typed (Prim l1 "PUSH" [Prim l2 "lambda" [a; b] n2; Seq l3 p] n1) s (Prim l4 "lambda" [a; b] n4 :: s)}
| Typed_pair {a b s} :
    `{instr_typed (Prim l1 "PAIR" [] n1) (a :: b :: s) (Prim l2 "pair" [a; b] n2 :: s)}
| Typed_pairN {k t ts s} :
    `{k >= 2 ->
      List.length ts = k ->
      comb_ty t ts ->
      instr_typed (Prim l1 "PAIR" [Int l2 (Z.of_nat k)] n1) (ts ++ s) (t :: s)}
| Typed_car {a b s} :
    `{instr_typed (Prim l1 "CAR" [] n1) (Prim l2 "pair" [a; b] n2 :: s) (a :: s)}
| Typed_cdr {a b s} :
    `{instr_typed (Prim l1 "CDR" [] n1) (Prim l2 "pair" [a; b] n2 :: s) (b :: s)}
| Typed_unpair {a b s} :
    `{instr_typed (Prim l1 "UNPAIR" [] n1) (Prim l2 "pair" [a; b] n2 :: s) (a :: b :: s)}
| Typed_unpairN {k t ts s} :
    `{k >= 2 ->
      List.length ts = k ->
      comb_ty t ts ->
      instr_typed (Prim l1 "UNPAIR" [Int l2 (Z.of_nat k)] n1) (t :: s) (ts ++ s)}
| Typed_unit {s} :
    `{instr_typed (Prim l1 "UNIT" [] n1) s (Prim l2 "unit" [] n2 :: s)}
| Typed_left {a b s} :
    `{instr_typed (Prim l1 "LEFT" [b] n1) (a :: s) (Prim l2 "or" [a; b] n2 :: s)}
| Typed_right {a b s} :
    `{instr_typed (Prim l1 "RIGHT" [a] n1) (b :: s) (Prim l2 "or" [a; b] n2 :: s)}
| Typed_if_left {a b bt bf s1 s2} :
    `{prog_typed bt (a :: s1) s2 ->
      prog_typed bf (b :: s1) s2 ->
      instr_typed (Prim l1 "IF_LEFT" [Seq l2 bt; Seq l3 bf] n1) (Prim l4 "or" [a; b] n4 :: s1) s2}
| Typed_if {bt bf s1 s2} :
    `{prog_typed bt s1 s2 ->
      prog_typed bf s1 s2 ->
      instr_typed (Prim l1 "IF" [Seq l2 bt; Seq l3 bf] n1) (Prim l4 "bool" [] n4 :: s1) s2}
| Typed_if_none {a bt bf s1 s2} :
    `{prog_typed bt s1 s2 ->
      prog_typed bf (a :: s1) s2 ->
      instr_typed (Prim l1 "IF_NONE" [Seq l2 bt; Seq l3 bf] n1) (Prim l4 "option" [a] n4 :: s1) s2}
| Typed_if_cons {a bt bf s1 s2} :
    `{prog_typed bt (a :: Prim l1 "list" [a] n1 :: s1) s2 ->
      prog_typed bf s1 s2 ->
      instr_typed (Prim l2 "IF_CONS" [Seq l3 bt; Seq l4 bf] n2) (Prim l5 "list" [a] n5 :: s1) s2}
| Typed_iter {elem coll body s} :
    `{iter_class elem coll ->
      prog_typed body (elem :: s) s ->
      instr_typed (Prim l1 "ITER" [Seq l2 body] n1) (coll :: s) s}
| Typed_map {elem elem' coll coll' body s} :
    `{map_class elem elem' coll coll' ->
      prog_typed body (elem :: s) (elem' :: s) ->
      instr_typed (Prim l1 "MAP" [Seq l2 body] n1) (coll :: s) (coll' :: s)}
| Typed_loop_left {a b body s} :
    `{prog_typed body (a :: s) (Prim l1 "or" [a; b] n1 :: s) ->
      instr_typed (Prim l2 "LOOP_LEFT" [Seq l3 body] n2) (Prim l4 "or" [a; b] n4 :: s) (b :: s)}
| Typed_failwith {a s1 s2} :
    `{instr_typed (Prim l1 "FAILWITH" [] n1) (a :: s1) s2}
with prog_typed : list (node A string) -> list (node A string) -> list (node A string) -> Prop :=
| Typed_nil {s} :
    prog_typed [] s s
| Typed_cons {i p s1 s2 s3} :
    instr_typed i s1 s2 -> prog_typed p s2 s3 ->
    prog_typed (i :: p) s1 s3
.

Fixpoint weak_instr {i s1 s2 r} (Htyped : instr_typed i s1 s2) {struct Htyped} :
  instr_typed i (s1 ++ r) (s2 ++ r)
with weak_prog {p s1 s2 r} (Htyped : prog_typed p s1 s2) {struct Htyped} :
  prog_typed p (s1 ++ r) (s2 ++ r).
Proof.
  - destruct i;
      inversion Htyped; subst;
        clear weak_instr; simpl; eauto;
          (* DIG/DUG *)
          try solve [repeat rewrite <- app_assoc; econstructor; eauto];
          (* etc *)
          try solve [econstructor; eauto; repeat rewrite app_comm_cons; eauto].
  - destruct p;
      inversion Htyped; subst.
    + constructor.
    + econstructor.
      apply weak_instr; eassumption.
      apply weak_prog; eassumption.
Qed.

End instr.

Hint Constructors comb_ty.
Hint Constructors instr_typed.
Hint Constructors prog_typed.
Hint Resolve weak_prog.
