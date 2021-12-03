Local Set Warnings "-implicit-core-hint-db".
Set Implicit Arguments.
From Coq Require Import String List Arith ZArith Lia.
Import ListNotations.
Open Scope string_scope.

From ligo_coq Require Import lemmas co_de_bruijn types ligo michelson micheline.

Section compiler.

(**************
 * Hypotheses *
 **************)

Context {A : Set}.
Context {op : Set}.
Context {lit : Set}.
Context {nil : A}.
Context {op_code : A -> op -> @static_args A op lit -> list (node A string)}.
Context {op_typed : op -> @static_args A op lit -> list (node A string) -> (node A string) -> Prop}.
Context {op_code_type_preservation :
  forall l o sargs az b,
    op_typed o sargs az b ->
    prog_typed (op_code l o sargs) az [b]}.
Context {lit_type : lit -> node A string}.
Context {lit_value : lit -> node A string}.
Context {lit_type_preservation :
  forall loc l s,
    prog_typed [Prim loc "PUSH" [lit_type l; lit_value l] []] s (lit_type l :: s)}.

Hint Resolve op_code_type_preservation.
Hint Resolve lit_type_preservation.

(************
 * Programs *
 ************)

Fixpoint compile_usages_aux (n : nat) (us : list usage) : list (node A string) :=
  match us with
  | [] => []
  | Drop :: us => [Prim nil "DIG" [Int nil (Z.of_nat n)] [];
                   Seq nil (compile_usages_aux (S n) us);
                   Prim nil "DROP" [] []]
  | Keep :: us => compile_usages_aux (S n) us
  end.

Definition compile_usages (us : list usage) : list (node A string) :=
  compile_usages_aux O us.

Fixpoint compile_splitting_aux (n : nat) (ss : splitting) : list (node A string) :=
  match ss with
  | [] => []
  | Left :: ss => Prim nil "DIG" [Int nil (Z.of_nat (n + length ss))] [] :: compile_splitting_aux (S n) ss
  | Right :: ss => compile_splitting_aux n ss
  | Both :: ss => [Prim nil "DIG" [Int nil (Z.of_nat (n + length ss))] [];
                   Prim nil "DUP" [] [];
                   Prim nil "DUG" [Int nil (Z.of_nat (S n + length ss))] []]
                  ++ compile_splitting_aux (S n) ss
  end.

Definition compile_splitting (ss : splitting) : list (node A string) :=
  compile_splitting_aux O (rev ss).

Fixpoint comb (az : list (node A string)) : (node A string) :=
  match az with
  | [] => Prim nil "unit" [] []
  | [a] => a
  | a :: az => Prim nil "pair" [a; comb az] []
  end.

Definition PAIR (n : nat) : list (node A string) :=
  match n with
  | 0 => [Prim nil "UNIT" [] []]
  | 1 => []
  | _ => [Prim nil "PAIR" [Int nil (Z.of_nat n)] []]
  end.

(* Unfortunately there is a bug in the typechecking of `PAIR k` which
   makes it difficult to use in general. This is a workaround.

   Note that it takes its arguments in reverse order. *)
Definition REV_PAIR (n : nat) : list (node A string) :=
  match n with
  | 0 => [Prim nil "UNIT" [] []]
  | 1 => []
  | _ => List.concat (repeat [Prim nil "SWAP" [] []; Prim nil "PAIR" [] []] (n - 1))
  end.

Definition UNPAIR (n : nat) : list (node A string) :=
  match n with
  | 0 => [Prim nil "DROP" [] []]
  | 1 => []
  | _ => [Prim nil "UNPAIR" [Int nil (Z.of_nat n)] []]
  end.

Definition GET (i n : nat) : list (node A string) :=
  let i := if beq_nat (S i) n
           then 2 * i
           else 2 * i + 1 in
  [Prim nil "GET" [Int nil (Z.of_nat i)] []].

Definition UPDATE (i n : nat) : list (node A string) :=
  let i := if beq_nat (S i) n
           then 2 * i
           else 2 * i + 1 in
  [Prim nil "UPDATE" [Int nil (Z.of_nat i)] []].

Fixpoint compile_expr
  (env : list (node A string)) (outer : splitting)
  (e : expr) {struct e} : list (node A string) :=
  match e with
  | E_var _ => compile_splitting outer
  | E_let_in _ inner e1 e2 =>
    let (env1, env2) := split inner env in
    let (outer, inner) := assoc_splitting outer inner in
    [Seq nil (compile_expr env1 outer e1);
     Seq nil (compile_binds env2 inner (filter_keeps (right_usages outer)) e2)]
  (* TODO use PAIR instead of REV_PAIR when possible *)
  | E_tuple _ args =>
    [Seq nil (compile_args env outer args);
     Seq nil (REV_PAIR (args_length args))]
  | E_let_tuple _ inner e1 e2 =>
    let (env1, env2) := split inner env in
    let (outer, inner) := assoc_splitting outer inner in
    [Seq nil (compile_expr env1 outer e1);
     Seq nil (UNPAIR (binds_length e2));
     Seq nil (compile_binds env2 inner (filter_keeps (right_usages outer)) e2)]
  | E_proj _ e i n =>
    [Seq nil (compile_expr env outer e);
     Seq nil (GET i n)]
  | E_update _ args i n =>
    [Seq nil (compile_args env outer args);
     Seq nil (UPDATE i n)]
  | E_app _ e => [Seq nil (compile_args env outer e);
                  Prim nil "SWAP" [] [];
                  Prim nil "EXEC" [] []]
  | E_lam _ e b =>
    let a := match e with
             | Binds _ [a] _ => a
             | _ => Prim nil "unit" [] []
             end in
    match env with
    | [] =>
      [Seq nil (compile_splitting outer);
       Prim nil "LAMBDA" [a; b; Seq nil (compile_binds [] [] [] e)] []]
    | _ :: _ =>
      let body :=
        (* optimize the case when fun arg is not used *)
        match e with
        | Binds [Drop] [a] e =>
          [Prim nil "CAR" [] [];
           Seq nil (UNPAIR (length env));
           Seq nil (compile_expr env (repeat Left (length env)) e)]
        | _ =>
          (* TODO can we rearrange things so this is just UNPAIR (S (length env)) ? *)
          [Seq nil [Prim nil "DUP" [] [];
                    Prim nil "CDR" [] [];
                    Prim nil "SWAP" [] [];
                    Prim nil "CAR" [] []];
           Seq nil (UNPAIR (length env));
           Prim nil "DIG" [Int nil (Z.of_nat (length env))] [];
           Seq nil (compile_binds env (repeat Left (length env)) (repeat Keep (length env)) e)]
        end in
      [Seq nil (compile_splitting outer);
       Seq nil (PAIR (length env));
       Prim nil "LAMBDA" [Prim nil "pair" [comb env; a] []; b;
                          Seq nil body]
                         [];
       Prim nil "SWAP" [] [];
       Prim nil "APPLY" [] []]
    end
  | E_operator l op sargs args =>
    [Seq nil (compile_args env outer args);
     Seq nil (op_code l op sargs)]
  | E_literal _ lit =>
    [Prim nil "PUSH" [lit_type lit; lit_value lit] []]
  | E_pair _ e => [Seq nil (compile_args env outer e); Prim nil "PAIR" [] []]
  | E_car _ e => [Seq nil (compile_expr env outer e); Prim nil "CAR" [] []]
  | E_cdr _ e => [Seq nil (compile_expr env outer e); Prim nil "CDR" [] []]
  | E_unit _ => [Seq nil (compile_splitting outer); Prim nil "UNIT" [] []]
  | E_left _ b e => [Seq nil (compile_expr env outer e); Prim nil "LEFT" [b] []]
  | E_right _ a e => [Seq nil (compile_expr env outer e); Prim nil "RIGHT" [a] []]
  | E_if_bool _ e => let '(e1, e2, e3) := compile_cond env outer e in
                     [Seq nil e1; Prim nil "IF" [Seq nil e2; Seq nil e3] []]
  | E_if_none _ e => let '(e1, e2, e3) := compile_cond env outer e in
                     [Seq nil e1; Prim nil "IF_NONE" [Seq nil e2; Seq nil e3] []]
  | E_if_cons _ e => let '(e1, e2, e3) := compile_cond env outer e in
                     [Seq nil e1; Prim nil "IF_CONS" [Seq nil e2; Seq nil e3] []]
  | E_if_left _ e => let '(e1, e2, e3) := compile_cond env outer e in
                     [Seq nil e1; Prim nil "IF_LEFT" [Seq nil e2; Seq nil e3] []]
  | E_iter _ inner e1 e2 =>
    let (env1, env2) := split inner env in
    let inner := flip_splitting inner in
    let (outer, inner) := assoc_splitting outer inner in
    [Seq nil (compile_expr env2 outer e2);
     Prim nil "ITER" [Seq nil [Seq nil (compile_binds env1 (keep_rights (left_usages inner)) (filter_keeps (right_usages outer)) e1);
                               Prim nil "DROP" [] []]] [];
     Seq nil (compile_usages (right_usages inner));
     Prim nil "UNIT" [] []]
  | E_map _ inner e1 e2 =>
    let (env1, env2) := split inner env in
    let inner := flip_splitting inner in
    let (outer, inner) := assoc_splitting outer inner in
    [Seq nil (compile_expr env2 outer e2);
     Prim nil "MAP" [Seq nil (compile_binds env1 (keep_rights (left_usages inner)) (filter_keeps (right_usages outer)) e1)] [];
     Seq nil (compile_usages (Keep :: right_usages inner))]
  | E_loop_left _ inner e1 b e2 =>
    let (env1, env2) := split inner env in
    let inner := flip_splitting inner in
    let (outer, inner) := assoc_splitting outer inner in
    [Seq nil (compile_expr env2 outer e2);
     Prim nil "LEFT" [b] [];
     Prim nil "LOOP_LEFT" [Seq nil (compile_binds env1 (keep_rights (left_usages inner)) (filter_keeps (right_usages outer)) e1)] [];
     Seq nil (compile_usages (Keep :: right_usages inner))]
  | E_fold _ inner1 e1 inner2 e2 e3 =>
    let (env1, env') := split inner1 env in
    let (env2, env3) := split inner2 env' in
    let (outer, inner1) := assoc_splitting outer inner1 in
    let (inner1, inner2) := assoc_splitting inner1 inner2 in
    [Seq nil (compile_expr env1 outer e1);
     Seq nil (compile_expr env2 (Right :: inner1) e2);
     Prim nil "ITER" [Seq nil [Prim nil "SWAP" [] []; Prim nil "PAIR" [] [];
                               Seq nil (compile_binds env3 (keep_rights (left_usages inner2)) (filter_keeps (right_usages inner1)) e3)]] [];
     Seq nil (compile_usages (Keep :: right_usages inner2))]
  | E_fold_right _ elem inner1 e1 inner2 e2 e3 =>
    let (env1, env') := split inner1 env in
    let (env2, env3) := split inner2 env' in
    let (outer, inner1) := assoc_splitting outer inner1 in
    let (inner1, inner2) := assoc_splitting inner1 inner2 in
    [Seq nil (compile_expr env1 outer e1);
     Seq nil (compile_expr env2 (Right :: inner1) e2);
     (* build reversed list: *)
     Seq nil [Prim nil "NIL" [elem] [];
              Prim nil "SWAP" [] [];
              Prim nil "ITER" [Seq nil [Prim nil "CONS" [] []]] []];
     (* fold over reversed list: *)
     Prim nil "ITER" [Seq nil [Prim nil "PAIR" [] [];
                               Seq nil (compile_binds env3 (keep_rights (left_usages inner2)) (filter_keeps (right_usages inner1)) e3)]] [];
     Seq nil (compile_usages (Keep :: right_usages inner2))]
  | E_failwith x e => [Seq nil (compile_expr env outer e); Prim x "FAILWITH" [] []]
  | E_raw_michelson _ a b code => [Prim nil "PUSH" [Prim nil "lambda" [a; b] []; Seq nil code] []]
  end
with
compile_args
  (env : list (node A string)) (outer : splitting)
  (e : args) {struct e} : list (node A string) :=
  match e with
  | Args_nil => []
  | Args_cons inner e args =>
    let (env1, env2) := split inner env in
    let (outer', inner') := assoc_splitting outer inner in
    [Seq nil (compile_expr env1 outer' e);
     Seq nil (compile_args env2 (Right :: inner') args)]
  end
with
compile_binds
  (env : list (node A string)) (outer : splitting) (proj : list usage)
  (e : binds) {struct e} : list (node A string) :=
  match e with
  | Binds us az e =>
    let env' := app (select us az) env in
    let outer' := app (repeat Left (length (select us az))) outer in
    [Seq nil (compile_usages (us ++ proj));
     Seq nil (compile_expr env' outer' e)]
  end
with
compile_cond
  (env : list (node A string)) (outer : splitting)
  (e : cond) {struct e} : list (node A string) * list (node A string) * list (node A string) :=
  match e with
  | Cond inner1 e1 inner2 b2 b3 =>
    let (env1, env') := split inner1 env in
    let (env2, env3) := split inner2 env' in
    let (outer', inner1') := assoc_splitting outer inner1 in
    let (outerR, innerR) := assoc_splitting inner1' inner2 in
    let (outerL, innerL) := assoc_splitting inner1' (flip_splitting inner2) in
    (compile_expr env1 outer' e1,
     compile_binds env2 innerL (right_usages outerL) b2,
     compile_binds env3 innerR (right_usages outerR) b3)
  end.


(**********
 * Proofs *
 **********)

Lemma compile_usages_aux_typed :
  forall us n s1 s2,
    length s1 = n ->
    length us = length s2 ->
    prog_typed (compile_usages_aux n us) (s1 ++ s2) (s1 ++ select us s2).
Proof.
  intros us; induction us; intros n s1 s2 H1 H2; eauto;
    destruct s2; inversion H2 as [H3];
      simpl; eauto; destruct a; simpl; eauto.
  (* Drop *)
  - enough (prog_typed (compile_usages_aux (S n) us) ((n0 :: s1) ++ s2) ((n0 :: s1) ++ select us s2))
      by eauto 10;
    eapply IHus; simpl; eauto.
  (* Keep *)
  - enough (H : prog_typed (compile_usages_aux (S n) us) ((s1 ++ [n0]) ++ s2) ((s1 ++ [n0]) ++ select us s2))
      by (repeat rewrite <- app_assoc in H; apply H);
    eapply IHus; eauto; rewrite length_snoc; eauto.
Qed.

Lemma compile_usages_typed :
  forall {us g g'},
    used us g' g ->
    prog_typed (compile_usages us) g g'.
Proof.
  intros; rewrite <- select_iff_used in H; destruct H; subst;
    apply compile_usages_aux_typed with (s1 := []); auto.
Qed.

Hint Resolve compile_usages_typed.

Lemma invert_compile_usages_aux_keeps_typed :
  forall m n s1 s2,
    prog_typed (compile_usages_aux n (repeat Keep m)) s1 s2 ->
    s1 = s2.
Proof.
  induction m; intros;
    try solve [ inversion H; auto ].
  simpl in H; eapply IHm; eauto.
Qed.

Definition destruct_last :
  forall (A : Type) (l : list A),
    {x : A & {xs : list A | l = app xs [x]}} + {l = []}.
Proof.
  intros A' l; induction l; auto;
    left; destruct IHl as [[x [xs H]]|H].
  - exists x; exists (a :: xs); rewrite H; auto.
  - rewrite H; exists a; exists []; auto.
Defined.

Lemma compile_splitting_aux_typed :
  forall ss n s1 s2,
    length s1 = n ->
    length ss = length s2 ->
    let ss' := rev ss in
    prog_typed (compile_splitting_aux n ss) (s1 ++ s2) (select (left_usages ss') s2 ++ s1 ++ select (right_usages ss') s2).
Proof.
  intros ss; induction ss; intros n s1 s2 H1 H2.
  - destruct s2; inversion H2; simpl; eauto.
  - destruct a;
      pose proof (destruct_last s2) as H3;
      destruct H3 as [[x [s2' H3]]|H3];
      try (solve [subst; inversion H2]);
      subst;
      rewrite app_length, plus_comm in H2;
      inversion H2 as [H3];
      simpl;
      rewrite left_usages_app, right_usages_app;
      rewrite select_app by (auto; rewrite left_usages_length; rewrite rev_length; auto);
      rewrite select_app by (auto; rewrite right_usages_length; rewrite rev_length; auto).
    (* left *)
    + econstructor.
      * rewrite H3, app_assoc, <- app_length; auto.
      * simpl;
          repeat rewrite app_nil_r;
          rewrite <- app_assoc;
          change (prog_typed (compile_splitting_aux (S (length s1)) ss) ((x :: s1) ++ s2') (select (left_usages (rev ss)) s2' ++ (x :: s1) ++ select (right_usages (rev ss)) s2'));
          apply IHss; auto.
    (* right *)
    + simpl.
      rewrite app_nil_r.
      repeat rewrite app_assoc.
      apply weak_prog.
      rewrite <- app_assoc.
      apply IHss; auto.
    (* both *)
    + econstructor.
      * rewrite H3, app_assoc, <- app_length; auto.
      * econstructor; auto.
        { econstructor.
          - rewrite H3, <- app_length.
            enough (instr_typed (Prim nil "DUG" [Int nil (Z.of_nat (Datatypes.length (x :: s1 ++ s2')))] []) (x :: (x :: s1 ++ s2') ++ []) ((x :: s1 ++ s2') ++ [x])) by eassumption.
            eapply Typed_dug; eauto.
          - simpl.
            repeat rewrite app_assoc.
            rewrite app_comm_cons.
            apply weak_prog.
            repeat rewrite <- app_assoc.
            change (prog_typed (compile_splitting_aux (S (length s1)) ss) ((x :: s1) ++ s2') (select (left_usages (rev ss)) s2' ++ (x :: s1) ++ select (right_usages (rev ss)) s2')).
            apply IHss; auto. }
Qed.

Lemma compile_splitting_typed
      (ss : splitting) (d1 g d2 : list (node A string)) :
  splits ss d1 g d2 ->
  prog_typed (compile_splitting ss) d1 (g ++ d2).
Proof.
  intros; rewrite <- select_iff_splits in H; destruct H as [Hlen [H1 H2]]; subst.
  pose proof (@compile_splitting_aux_typed (rev ss) O [] d1 eq_refl) as H.
  simpl in H.
  rewrite rev_involutive, rev_length in H.
  apply H; auto.
Qed.

Lemma compile_splitting_typed_cons
      (ss : splitting) (d1 : list (node A string)) (a : (node A string)) (d2 : list (node A string)) :
  splits ss d1 [a] d2 ->
  prog_typed (compile_splitting ss) d1 (a :: d2).
Proof. apply compile_splitting_typed. Qed.

Hint Resolve compile_splitting_typed.
Hint Resolve compile_splitting_typed_cons.

Lemma comb_comb_ty :
  forall g t1 t2,
    comb_ty (comb (t1 :: t2 :: g)) (t1 :: t2 :: g).
Proof.
  intros g;
  induction g;
  intros t1 t2.
  - simpl; eauto.
  - simpl; simpl in IHg; eauto.
Qed.

Lemma PAIR_typed' :
  forall g g',
    @prog_typed A (PAIR (length g)) (g ++ g') (comb g :: g').
Proof.
  intros g g';
  destruct g as [|t1 g]; simpl; eauto;
  destruct g as [|t2 g]; simpl; eauto;
  econstructor; eauto;
  match goal with
  | [|- instr_typed (Prim ?l1 "PAIR" [Int ?l2 _] []) (?t1 :: ?t2 :: ?g ++ ?g') ?s'] =>
    change (instr_typed (Prim l1 "PAIR" [Int l2 (Z.of_nat (2 + length g))] []) ((t1 :: t2 :: g) ++ g') s')
  end;
  eapply Typed_pairN; eauto; try lia; eapply comb_comb_ty.
Qed.

Lemma UNPAIR_typed' :
  forall g g',
    @prog_typed A (UNPAIR (length g)) (comb g :: g') (g ++ g').
Proof.
  intros g g';
  destruct g as [|t1 g]; simpl; eauto;
  destruct g as [|t2 g]; simpl; eauto;
  econstructor; eauto.
  match goal with
  | [|- instr_typed (Prim ?l1 "UNPAIR" [Int ?l2 _] []) ?s (?t1 :: ?t2 :: ?g ++ ?g')] =>
    change (instr_typed (Prim l1 "UNPAIR" [Int l2 (Z.of_nat (2 + length g))] []) s ((t1 :: t2 :: g) ++ g'))
  end;
  eapply Typed_unpairN; eauto; try lia; eapply comb_comb_ty.
Qed.

Hint Resolve PAIR_typed'.
Hint Resolve UNPAIR_typed'.

Definition expr_type_preservation (e : expr) : Prop :=
  forall (g : list (node A string)) (a : (node A string))
         (Htyped : @expr_typed A op lit op_typed lit_type g e a)
         (outer : splitting) (d1 d2 : list (node A string))
         (Hsplits : splits outer d1 g d2),
    prog_typed (compile_expr g outer e) d1 (a :: d2).

Definition args_type_preservation (e : args) : Prop :=
  forall (g : list (node A string)) (d : list (node A string))
         (Htyped : @args_typed A op lit op_typed lit_type g e d)
         (outer : splitting) (d1 d2 : list (node A string))
         (Hsplits : splits outer d1 g d2),
    prog_typed (compile_args g outer e) d1 (d ++ d2).

Definition binds_type_preservation (e : binds) : Prop :=
  forall (g : list (node A string)) (az : list (node A string)) (b : (node A string))
         (Htyped : @binds_typed A op lit op_typed lit_type g e az b)
         (outer : splitting) (proj : list usage) (d1 d1' d2 : list (node A string))
         (Hsplits : splits outer d1 g d2)
         (Hused : used proj d1 d1'),
    prog_typed (compile_binds g outer proj e) (az ++ d1') (b :: d2).

Definition cond_type_preservation (e : cond) : Prop :=
 forall (g : list (node A string)) (a : (node A string)) (bs cs : list (node A string)) (d : (node A string))
        (Htyped : @cond_typed A op lit op_typed lit_type g e a bs cs d)
        (outer : splitting) (d1 d2 : list (node A string))
        (Hsplits : splits outer d1 g d2),
 exists d',
   prog_typed (fst (fst (compile_cond g outer e))) d1 (a :: d') /\
   prog_typed (snd (fst (compile_cond g outer e))) (bs ++ d') (d :: d2) /\
   prog_typed      (snd (compile_cond g outer e))  (cs ++ d') (d :: d2).

Definition static_args_type_preservation (e : static_args) : Prop :=
  forall (Htyped : @static_args_typed A op lit op_typed lit_type e), True.

Definition script_type_preservation (e : script) : Prop :=
  forall (Htyped : @script_typed A op lit op_typed lit_type e), True.

Ltac rewriteHyp :=
  match goal with
    | [ H : _ |- _ ] => rewrite H by solve [ auto ]
  end.
Ltac rewriterP := repeat (rewriteHyp; autorewrite with core in *).
Ltac rewriter := autorewrite with core in *; rewriterP.

(* http://poleiro.info/posts/2018-10-15-checking-for-constructors.html *)
Ltac head t :=
  (* slightly modified, we will evaluate to get a head *)
  let t := eval hnf in t in
  match t with
  | ?t' _ => head t'
  | _ => t
  end.

Ltac head_constructor t :=
  let t' := head t in is_constructor t'.

Ltac invert H := inversion H; subst; clear H.

Ltac crush :=
  (rewriter;
  match goal with
   | [|- context[let (s, u) := ?z in _]] =>
     rewrite (surjective_pairing z)
   | [H : context[let (s, u) := ?z in _] |- _] =>
     rewrite (surjective_pairing z) in H
   (* hmm *)
   | [H : context[fst (assoc_splitting ?outer ?inner)] |- _] =>
     let eq := fresh "eq" in
     assert (eq : fst (assoc_splitting outer inner) = assoc_splitting1 outer inner) by reflexivity;
     rewrite eq in H; clear eq
   | [|- context[fst (assoc_splitting ?outer ?inner)]] =>
     let eq := fresh "eq" in
     assert (eq : fst (assoc_splitting outer inner) = assoc_splitting1 outer inner) by reflexivity;
     rewrite eq; clear eq
   | [H : context[snd (assoc_splitting ?outer ?inner)] |- _] =>
     let eq := fresh "eq" in
     assert (eq : snd (assoc_splitting outer inner) = assoc_splitting2 outer inner) by reflexivity;
     rewrite eq in H; clear eq
   | [|- context[snd (assoc_splitting ?outer ?inner)]] =>
     let eq := fresh "eq" in
     assert (eq : snd (assoc_splitting outer inner) = assoc_splitting2 outer inner) by reflexivity;
     rewrite eq; clear eq
   | [H : splits _ _ _ _ |- _] =>
     let Ha := fresh "H" in
     let Hb := fresh "H" in
     let Hc := fresh "H" in
     rewrite <- select_iff_splits in H; destruct H as [Ha [Hb Hc]]; subst
   | [H : used _ _ _ |- _] =>
     let Ha := fresh "H" in
     let Hb := fresh "H" in
     rewrite <- select_iff_used in H; destruct H as [Ha Hb]
   | [H : prog_typed ?p _ _ |- _] =>
     head_constructor p; invert H
   | [H : instr_typed ?i _ _ |- _] =>
     head_constructor i; invert H
   | [H : used ?u _ _ |- _] =>
     head_constructor u; invert H
   | [H : @eq (list _) (?xs ++ _)%list (_ :: _) |- _] =>
     destruct xs; invert H
   | [H : @eq (list _) (_ :: _) (?xs ++ _)%list |- _] =>
     destruct xs; invert H
   | [H : @eq (list _) (_ :: _) ?xs |- _] =>
     subst
   | [H : @eq Z.t ?x ?y |- _] =>
     discriminate H
  end).

(* really need to stop mentioning hypotheses by generated names *)
Lemma type_preservation :
  (forall e, expr_type_preservation e) /\
  (forall e, args_type_preservation e) /\
  (forall e, binds_type_preservation e) /\
  (forall e, cond_type_preservation e) /\
  (forall e, static_args_type_preservation e) /\
  (forall e, script_type_preservation e).
Proof.
  apply expr_mutind;
    unfold expr_type_preservation;
    unfold args_type_preservation;
    unfold binds_type_preservation;
    unfold cond_type_preservation;
    unfold static_args_type_preservation;
    unfold script_type_preservation;
    intros; inversion Htyped; subst; clear Htyped;
      try solve [simpl in *; eauto].
  (* E_let_in *)
  - pose proof (assoc_splits Hsplits H7) as H23; destruct H23 as [d'' [H23a H23b]].
    eapply H in H8; eauto.
    simpl; repeat crush.
    econstructor.
    + eauto.
    + econstructor.
      econstructor.
      specialize (H0 _ _ _ H9); eapply H0; eauto.
      eapply used_filter_keeps_right_usages; eauto.
      eauto.
  (* E_tuple *)
  - admit. (* TODO *)
  (* E_let_tuple *)
  - admit. (* TODO *)
  (* E_proj *)
  - admit. (* TODO *)
  (* E_update *)
  - admit. (* TODO *)
  (* E_app *)
  - eapply H in H4; eauto; simpl in *; eauto.
  (* E_lam *)
  - simpl.
    destruct g.
    + econstructor; eauto;
        inversion H5; subst;
        specialize (H _ _ _ H5);
        econstructor; eauto;
        econstructor; eauto;
        eapply H with (d1' := []); eauto.
    (* hmm *)
    + econstructor; eauto;
        inversion H5; subst.
      econstructor. eauto.
      unshelve (econstructor; eauto); try (eapply nil); try (exact []).
      shelve.
      destruct us as [|u us]; try (destruct u); try (destruct us as [|u' us]);
        try solve [ econstructor; econstructor;
                    eauto 10;
                    econstructor; eauto; econstructor; eauto;
                    econstructor; eauto;
                    econstructor;
                    rewrite app_nil_r;
                    specialize (H _ _ _ H5);
                    specialize (H (repeat Left (length (n0 :: g))) (repeat Keep (length (n0 :: g))) (n0 :: g) (n0 :: g) []);
                    eapply H; eauto ].
      (* optimized case for unused argument + closure, awkward because
         compiler does not follow the induction principle for
         expressions in this case *)
      econstructor;
        econstructor; eauto;
        econstructor; eauto;
        econstructor; eauto;
        econstructor;
        rewrite app_nil_r;
        specialize (H _ _ _ H5
                      (repeat Left (length (n0 :: g))) (repeat Keep (length (n0 :: g)))
                      _ _ _
                      (splits_lefts' (n0 :: g)) (used_keeps (n0 :: g))).
      simpl in H.
      repeat crush;
        match goal with
        | [H : prog_typed (compile_usages_aux _ _) _ _ |- _] =>
          apply invert_compile_usages_aux_keeps_typed in H; simpl in H
        end;
        repeat crush;
        match goal with
        | [H : _ :: _ = _ :: _ |- _] => invert H
        end;
        eauto.
  (* E_operator *)
  - eapply H0 in H8; eauto;
      simpl; eauto 7.
  (* E_literal *)
  - apply splits_right in Hsplits; subst; simpl; eauto.
  (* E_pair *)
  - eapply H in H4; simpl in *; eauto.
  (* E_if_left *)
  - eapply H in H4; simpl in *; eauto.
    rewrite (surjective_pairing (compile_cond g outer c)).
    rewrite (surjective_pairing (fst (compile_cond g outer c))).
    destruct H4 as [d' [H3 [H4 H5]]].
    eauto.
  (* E_if *)
  - eapply H in H4; simpl in *; eauto.
    rewrite (surjective_pairing (compile_cond g outer c)).
    rewrite (surjective_pairing (fst (compile_cond g outer c))).
    destruct H4 as [d' [H3 [H4 H5]]].
    eauto.
  (* E_if_none *)
  - eapply H in H4; simpl in *; eauto.
    rewrite (surjective_pairing (compile_cond g outer c)).
    rewrite (surjective_pairing (fst (compile_cond g outer c))).
    destruct H4 as [d' [H3 [H4 H5]]].
    eauto.
  (* E_if_cons *)
  - eapply H in H4; simpl in *; eauto.
    rewrite (surjective_pairing (compile_cond g outer c)).
    rewrite (surjective_pairing (fst (compile_cond g outer c))).
    destruct H4 as [d' [H3 [H4 H5]]].
    eauto.
  (* E_iter *)
  - pose proof (assoc_splits Hsplits (flip_splits H6)) as H24; destruct H24 as [d'' [H24a H24b]].
    simpl; repeat crush.
    repeat (econstructor; [solve[eauto]|idtac]).
    econstructor; econstructor; eauto.
    econstructor. econstructor.
    specialize (H _ _ _ H9); eapply H.
    eapply splits_keep_rights_left_usages; eauto.
    eapply used_filter_keeps_right_usages; eauto.
    eauto.
  (* E_map *)
  - pose proof (assoc_splits Hsplits (flip_splits H6)) as H24; destruct H24 as [d'' [H24a H24b]].
    simpl; repeat crush.
    repeat (econstructor; [solve[eauto]|idtac]).
    econstructor; econstructor; eauto.
    specialize (H _ _ _ H9); eapply H.
    eapply splits_keep_rights_left_usages; eauto.
    eapply used_filter_keeps_right_usages; eauto.
    eauto.
    eauto 10.
  (* E_loop_left *)
  - pose proof (assoc_splits Hsplits (flip_splits H8)) as H24; destruct H24 as [d'' [H24a H24b]].
    simpl; repeat crush.
    unshelve (repeat (econstructor; [solve[eauto]|idtac])).
    apply nil. exact [].
    econstructor; econstructor; eauto.
    specialize (H _ _ _ H9); eapply H.
    eapply splits_keep_rights_left_usages; eauto.
    eapply used_filter_keeps_right_usages; eauto.
    eauto.
    eauto 10.
  (* E_fold *)
  - pose proof (assoc_splits Hsplits H9) as H24; destruct H24 as [d' [H24a H24b]].
    pose proof (assoc_splits H24b H11) as H25; destruct H25 as [d'' [H25a H25b]].
    simpl; repeat crush.
    repeat (econstructor; [solve[eauto]|idtac]).
    econstructor.
    econstructor; eauto.
    econstructor; eauto.
    econstructor; eauto.
    econstructor.
    econstructor. eapply H1 in H15. apply H15.
    eapply splits_keep_rights_left_usages; eauto.
    eapply used_filter_keeps_right_usages; eauto.
    eauto.
    eauto 10.
  (* E_fold_right *)
  - admit. (* TODO *)
  (* E_raw_michelson *)
  - apply splits_right in Hsplits; subst; simpl; eauto.
  (* Args_nil *)
  - apply splits_right in Hsplits; subst; simpl; eauto.
  (* Args_cons *)
  - rewrite <- app_assoc; simpl.
    pose proof (assoc_splits Hsplits H5) as H100; destruct H100 as [d' [H100a H100b]].
    repeat crush.
    eauto 10.
  (* Binds *)
  - simpl;
      repeat crush; subst;
        eapply H in H7; eauto 10;
          eapply splits_lefts; eauto.
  (* Cond *)
  - pose proof (assoc_splits Hsplits H8) as H23; destruct H23 as [d3 [H23a H23b]];
    pose proof (assoc_splits H23b H13) as H24; destruct H24 as [d4 [H24a H24b]];
    pose proof (assoc_splits H23b (flip_splits H13)) as H25; destruct H25 as [d5 [H25a H25]].
    simpl; repeat crush; subst;
      eexists; split; simpl.
    + eauto.
    + split.
      * eapply H0; eauto.
      * eapply H1; eauto.
        rewrite <- H7, <- H9.
        eauto.
Admitted. (* TODO *)

End compiler.
