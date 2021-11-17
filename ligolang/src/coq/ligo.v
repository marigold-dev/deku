Set Implicit Arguments.
From Coq Require Import String List ZArith.
Import ListNotations.

From ligo_coq Require Import co_de_bruijn types michelson micheline bytes.

Section expr.
Context {A : Set}.
Context {op : Set}.
Context {lit : Set}.

Context {raw_typed : node A string -> list (node A string) -> (node A string) -> Prop}.

Inductive expr : Set :=
| E_var : A -> expr
| E_let_in : A -> splitting -> expr -> binds -> expr

| E_tuple : A -> args -> expr
| E_let_tuple : A -> splitting -> expr -> binds -> expr
| E_proj : A -> expr -> nat -> nat -> expr
| E_update : A -> args -> nat -> nat -> expr

| E_app : A -> args -> expr
| E_lam : A -> binds -> node A string -> expr

| E_operator : A -> op -> static_args -> args -> expr
| E_literal : A -> lit -> expr

| E_pair : A -> args -> expr
| E_car : A -> expr -> expr
| E_cdr : A -> expr -> expr

| E_unit : A -> expr

| E_left : A -> node A string -> expr -> expr
| E_right : A -> node A string -> expr -> expr

| E_if_left : A -> cond -> expr
| E_if_bool : A -> cond -> expr
| E_if_none : A -> cond -> expr
| E_if_cons : A -> cond -> expr

| E_iter : A -> splitting -> binds -> expr -> expr
| E_map : A -> splitting -> binds -> expr -> expr
| E_loop_left : A -> splitting -> binds -> node A string -> expr -> expr
| E_fold : A -> splitting -> expr -> splitting -> expr -> binds -> expr
| E_fold_right : A -> node A string -> splitting -> expr -> splitting -> expr -> binds -> expr

| E_failwith : A -> expr -> expr

| E_raw_michelson : A -> node A string -> node A string -> list (node A string) -> expr

with args : Set :=
| Args_nil : args
| Args_cons : splitting -> expr -> args -> args

with binds : Set :=
| Binds : list usage -> list (node A string) -> expr -> binds

with cond : Set :=
| Cond : splitting -> expr -> splitting ->
         binds -> binds ->
         cond

with static_args : Set :=
| Type_args : option string -> list (node A string) -> static_args
| Script_arg : script -> static_args

with script : Set :=
| Script : node A string -> node A string -> binds -> script
.

Scheme expr_ind' := Induction for expr Sort Prop
with args_ind' := Induction for args Sort Prop
with binds_ind' := Induction for binds Sort Prop
with cond_ind' := Induction for cond Sort Prop
with static_args_ind' := Induction for static_args Sort Prop
with script_ind' := Induction for script Sort Prop
.

Combined Scheme expr_mutind from expr_ind', args_ind', binds_ind', cond_ind', static_args_ind', script_ind'.

Context {op_typed : op -> static_args -> list (node A string) -> node A string -> Prop}.
Context {lit_type : lit -> node A string}.

Local Open Scope string_scope.
Local Open Scope Z_scope.

Local Generalizable Variable l n.

(* inductive characterization of "comb" tuples generalized to 0-tuples
   (unit) and 1-tuples (arbitrary types). TODO should instead just use
   a tuple type, making the type translation to Michelson no longer
   the identity? *)
Inductive tuple : list (node A string) -> node A string -> Prop :=
| Tuple_nil :
    `{tuple [] (Prim l "unit" [] n)}
| Tuple_one {a} :
    `{tuple [a] a}
| Tuple_cons {a1 a2 az a2z'} :
    tuple (a2 :: az) a2z' ->
    `{tuple (a1 :: a2 :: az) (Prim l "pair" [a1; a2z'] n)}
.

Inductive expr_typed : list (node A string) -> expr -> node A string -> Prop :=
| E_var_typed {a} :
    `{expr_typed [a] (E_var l) a}
| E_let_in_typed {ss e1 e2 g g1 g2 a b} :
    `{splits ss g g1 g2 ->
      expr_typed g1 e1 a ->
      binds_typed g2 e2 [a] b ->
      expr_typed g (E_let_in l ss e1 e2) b}
| E_tuple_typed {g args az t} :
    `{tuple az t ->
      args_typed g args az ->
      expr_typed g (E_tuple l1 args) t}
| E_let_tuple_typed {ss g g1 g2 az azt c e1 e2} :
    `{splits ss g g1 g2 ->
      tuple az azt ->
      expr_typed g1 e1 azt ->
      binds_typed g2 e2 az c ->
      expr_typed g (E_let_tuple l3 ss e1 e2) c}
| E_proj_typed {g az azt a e i n} :
    `{tuple az azt ->
      List.nth_error az i = Some a ->
      List.length az = n ->
      expr_typed g e azt ->
      expr_typed g (E_proj l1 e i n) a}
| E_update_typed {g a az azt args i n} :
    `{tuple az azt ->
      List.nth_error az i = Some a ->
      List.length az = n ->
      args_typed g args [azt; a] ->
      expr_typed g (E_update l1 args i n) azt}
| E_app_typed {g args a b} :
    `{args_typed g args [Prim l1 "lambda" [a; b] n1; a] ->
      expr_typed g (E_app l2 args) b}
| E_lam_typed {g a b e} :
    `{binds_typed g e [a] b ->
      expr_typed g (E_lam l1 e b) (Prim l2 "lambda" [a; b] n2)}
| E_operator_typed {op sargs args g az b} :
    `{static_args_typed sargs ->
      args_typed g args az ->
      op_typed op sargs az b ->
      expr_typed g (E_operator l op sargs args) b}
| E_literal_typed {lit a} :
    `{lit_type lit = a ->
      expr_typed [] (E_literal l lit) a}
| E_pair_typed {g args a b} :
    `{args_typed g args [a; b] ->
      expr_typed g (E_pair l1 args) (Prim l2 "pair" [a; b] n2)}
| E_car_typed {g e a b} :
    `{expr_typed g e (Prim l1 "pair" [a; b] n1) ->
      expr_typed g (E_car l2 e) a}
| E_cdr_typed {g e a b} :
    `{expr_typed g e (Prim l1 "pair" [a; b] n1) ->
      expr_typed g (E_cdr l2 e) b}
| E_unit_typed :
    `{expr_typed [] (E_unit l1) (Prim l2 "unit" [] n2)}
| E_left_typed {g e a b} :
    `{expr_typed g e a ->
      expr_typed g (E_left l1 b e) (Prim l2 "or" [a; b] n2)}
| E_right_typed {g e a b} :
    `{expr_typed g e b ->
      expr_typed g (E_right l1 a e) (Prim l2 "or" [a; b] n2)}
| E_if_left_typed {g e a b c} :
    `{cond_typed g e (Prim l1 "or" [a; b] n1) [a] [b] c ->
      expr_typed g (E_if_left l2 e) c}
| E_if_bool_typed {g e c} :
    `{cond_typed g e (Prim l1 "bool" [] n1) [] [] c ->
      expr_typed g (E_if_bool l2 e) c}
| E_if_none_typed {g e a c} :
    `{cond_typed g e (Prim l1 "option" [a] n1) [] [a] c ->
      expr_typed g (E_if_none l2 e) c}
| E_if_cons_typed {g e a c} :
    `{cond_typed g e (Prim l1 "list" [a] n1) [a; Prim l2 "list" [a] n2] [] c ->
      expr_typed g (E_if_cons l3 e) c}
| E_iter_typed {ss g g1 g2 elem coll e1 e2} :
    `{splits ss g g1 g2 ->
      iter_class elem coll ->
      binds_typed g1 e1 [elem] (Prim l1 "unit" [] n1) ->
      expr_typed g2 e2 coll ->
      expr_typed g (E_iter l2 ss e1 e2) (Prim l3 "unit" [] n3)}
| E_map_typed {ss g g1 g2 elem elem' coll coll' e1 e2} :
    `{splits ss g g1 g2 ->
      map_class elem elem' coll coll' ->
      binds_typed g1 e1 [elem] elem' ->
      expr_typed g2 e2 coll ->
      expr_typed g (E_map l ss e1 e2) coll'}
| E_loop_left_typed {ss g g1 g2 a b e1 e2} :
    `{splits ss g g1 g2 ->
      binds_typed g1 e1 [a] (Prim l1 "or" [a; b] n1) ->
      expr_typed g2 e2 a ->
      expr_typed g (E_loop_left l2 ss e1 b e2) b}
| E_fold_typed {ss1 ss2 g g1 g' g2 g3 elem coll ret e1 e2 e3} :
    `{splits ss1 g g1 g' ->
      splits ss2 g' g2 g3 ->
      iter_class elem coll ->
      expr_typed g1 e1 ret ->
      expr_typed g2 e2 coll ->
      binds_typed g3 e3 [Prim l1 "pair" [ret; elem] n1] ret ->
      expr_typed g (E_fold l2 ss1 e1 ss2 e2 e3) ret}
(* same as E_fold but bound pair is flipped: *)
| E_fold_right_typed {ss1 ss2 g g1 g' g2 g3 elem coll ret e1 e2 e3} :
    `{splits ss1 g g1 g' ->
      splits ss2 g' g2 g3 ->
      iter_class elem coll ->
      expr_typed g1 e1 ret ->
      expr_typed g2 e2 coll ->
      binds_typed g3 e3 [Prim l1 "pair" [elem; ret] n1] ret ->
      expr_typed g (E_fold_right l2 elem ss1 e1 ss2 e2 e3) ret}
| E_failwith_typed {g a b e} :
    `{expr_typed g e a ->
      expr_typed g (E_failwith l1 e) b}
| E_raw_michelson_typed {code a b} :
    `{prog_typed code [a] [b] ->
      expr_typed [] (E_raw_michelson l1 a b code) (Prim l2 "lambda" [a; b] n2)}

with args_typed : list (node A string) -> args -> list (node A string) -> Prop :=
| Args_nil_typed :
    args_typed [] Args_nil []
| Args_cons_typed {env env1 env2 ss e args d a} :
    splits ss env env1 env2 ->
    expr_typed env1 e a ->
    args_typed env2 args d ->
    args_typed env (Args_cons ss e args) (d ++ [a])
with binds_typed : list (node A string) -> binds -> list (node A string) -> node A string -> Prop :=
| Binds_typed {env us az az' e b} :
    used us az' az ->
    expr_typed (az' ++ env) e b ->
    binds_typed env (Binds us az e) az b
with cond_typed : list (node A string) -> cond -> node A string -> list (node A string) -> list (node A string) -> node A string -> Prop :=
| Cond_typed {env env1 env' env2 env3 ss1 e1 ss2 b2 b3 a bs cs d} :
    splits ss1 env env1 env' ->
    splits ss2 env' env2 env3 ->
    expr_typed env1 e1 a ->
    binds_typed env2 b2 bs d ->
    binds_typed env3 b3 cs d ->
    cond_typed env (Cond ss1 e1 ss2 b2 b3) a bs cs d
with static_args_typed : static_args -> Prop :=
| Type_args_typed {ann az} :
    static_args_typed (Type_args ann az)
| Script_arg_typed {s} :
    script_typed s ->
    static_args_typed (Script_arg s)
with script_typed : script -> Prop :=
| Script_typed {p s e} :
    `{binds_typed [] e [Prim l1 "pair" [p; s] n1] (Prim l2 "pair" [Prim l3 "list" [Prim l4 "operation" [] n4] n3; s] n2) ->
      script_typed (Script p s e)}
.

Definition binds_length (e : binds) : nat :=
  match e with
  | Binds _ az _ => List.length az
  end.

Fixpoint args_length (e : args) : nat :=
  match e with
  | Args_nil => O
  | Args_cons _ _ e => S (args_length e)
  end.

End expr.

Arguments E_var {A op lit}.
Arguments E_app {A op lit}.
Arguments E_lam {A op lit}.
Arguments E_operator {A op lit}.
Arguments E_literal {A op lit}.
Arguments E_pair {A op lit}.
Arguments E_car {A op lit}.
Arguments E_cdr {A op lit}.
Arguments E_unit {A op lit}.
Arguments E_left {A op lit}.
Arguments E_right {A op lit}.
Arguments E_if_left {A op lit}.
Arguments E_iter {A op lit}.
Arguments E_map {A op lit}.
Arguments E_loop_left {A op lit}.
Arguments E_fold {A op lit}.
Arguments E_failwith {A op lit}.
Arguments E_raw_michelson {A op lit}.
Arguments Args_nil {A op lit}.
Arguments Args_cons {A op lit}.
Arguments Binds {A op lit}.
Arguments Cond {A op lit}.
Arguments Type_args {A op lit}.
Arguments Script_arg {A op lit}.
Arguments Script {A op lit}.
