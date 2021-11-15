Local Set Warnings "-implicit-core-hint-db".
Set Implicit Arguments.
From Coq Require Import List Program.Equality.
Import ListNotations.


Inductive usage : Set := Drop | Keep.

Inductive used {A : Type} : list usage -> list A -> list A -> Prop :=
| Used_nil : used [] [] []
| Used_drop {us y xs ys} : used us xs ys -> used (Drop :: us) xs (y :: ys)
| Used_keep {us y xs ys} : used us xs ys -> used (Keep :: us) (y :: xs) (y :: ys)
.

Hint Constructors used.

Lemma used_app {A : Type} {us1 us2 : list usage} {xs1 xs2 ys1 ys2 : list A} :
  used us1 xs1 ys1 ->
  used us2 xs2 ys2 ->
  used (us1 ++ us2) (xs1 ++ xs2) (ys1 ++ ys2).
Proof.
  intros H1; induction H1; intros H2; auto;
    constructor; eapply IHused; auto.
Qed.

Lemma used_drops {A : Type} (xs : list A) :
  used (repeat Drop (length xs)) [] xs.
Proof.
  induction xs; simpl; auto.
Qed.

Lemma used_keeps {A : Type} (xs : list A) :
  used (repeat Keep (length xs)) xs xs.
Proof.
  induction xs; simpl; auto.
Qed.

Hint Resolve used_app.
Hint Resolve used_drops.
Hint Resolve used_keeps.

Definition filter_keeps (us : list usage) : list usage :=
  filter (fun u => match u with | Keep => true | Drop => false end) us.

Fixpoint select {A : Type} (us : list usage) (xs : list A) : list A :=
  match (us, xs) with
  | ([], _) => xs
  | (_, []) => []
  | (Drop :: us, _ :: xs) => select us xs
  | (Keep :: us, x :: xs) => x :: select us xs
  end.

Lemma select_app :
  forall {A : Type} (us1 us2 : list usage) (xs1 xs2 : list A),
    length us1 = length xs1 ->
    length us2 = length xs2 ->
    select (us1 ++ us2) (xs1 ++ xs2) = select us1 xs1 ++ select us2 xs2.
Proof.
  intros A us1; induction us1; intros us2 xs1 xs2 Hlen1 Hlen2; simpl in *;
    destruct xs1; inversion Hlen1 as [Hlen1']; auto;
      destruct a; simpl; try (apply f_equal); auto.
Qed.

Inductive side : Set := Left | Right | Both.

Definition splitting := list side.

Inductive splits {A : Type} : list side -> list A -> list A -> list A -> Prop :=
| Splits_nil : splits [] [] [] []
| Splits_left {ss zs x xs ys} : splits ss zs xs ys -> splits (Left :: ss) (x :: zs) (x :: xs) ys
| Splits_right {ss zs xs y ys} : splits ss zs xs ys -> splits (Right :: ss) (y :: zs) xs (y :: ys)
| Splits_both {ss z zs xs ys} : splits ss zs xs ys -> splits (Both :: ss) (z :: zs) (z :: xs) (z :: ys)
.

Hint Constructors splits.

Lemma splits_right {A : Type} :
  forall (outer : splitting) (d1 d2 : list A),
    splits outer d1 [] d2 ->
    d1 = d2.
Proof.
  intros; dependent induction H; subst; auto;
    erewrite IHsplits by auto; auto.
Qed.

Lemma splits_right_rights {A : Type} :
  forall (outer : splitting) (d1 d2 : list A),
    splits outer d1 [] d2 ->
    outer = repeat Right (length d1).
Proof.
  intros; dependent induction H; auto;
    simpl; erewrite <- IHsplits by auto; auto.
Qed.

Lemma splits_lefts {A : Type} :
  forall (outer : splitting) (g g' d1 d2 : list A),
    splits outer d1 g d2 ->
    splits (repeat Left (length g') ++ outer) (g' ++ d1) (g' ++ g) d2.
Proof.
  intros outer g g'.
  generalize dependent g.
  generalize dependent outer.
  induction g'; intros; auto;
    apply IHg' in H; simpl; auto.
Qed.

Hint Resolve splits_lefts.

Lemma splits_lefts' {A : Type} :
  forall (g : list A),
    splits (repeat Left (length g)) g g [].
Proof.
  intros;
    enough (splits (repeat Left (length g) ++ []) (g ++ []) (g ++ []) [])
    by (repeat rewrite app_nil_r in H; auto);
    eapply splits_lefts; auto.
Qed.

Hint Resolve splits_lefts'.

Definition keep_right (u : usage) : side :=
  match u with
  | Drop => Right
  | Keep => Both
  end.

Definition keep_rights (us : list usage) : list side :=
  List.map keep_right us.

Definition keep_left (u : usage) : side :=
  match u with
  | Drop => Left
  | Keep => Both
  end.

Definition keep_lefts (us : list usage) : list side :=
  List.map keep_left us.

Definition left_usage (s : side) : usage :=
  match s with
  | Left => Keep
  | Right => Drop
  | Both => Keep
  end.

Definition right_usage (s : side) : usage :=
  match s with
  | Left => Drop
  | Right => Keep
  | Both => Keep
  end.

Definition left_usages (ss : splitting) : list usage := List.map left_usage ss.

Definition right_usages (ss : splitting) : list usage := List.map right_usage ss.

Definition usages (ss : splitting) : list usage * list usage :=
  (left_usages ss, right_usages ss).

Definition split {A : Type} (ss : splitting) (xs : list A) : list A * list A :=
  (select (left_usages ss) xs, select (right_usages ss) xs).

Lemma left_usages_app (ss ss' : splitting) :
  left_usages (ss ++ ss') = left_usages ss ++ left_usages ss'.
Proof. unfold left_usages; apply map_app. Qed.

Lemma right_usages_app (ss ss' : splitting) :
  right_usages (ss ++ ss') = right_usages ss ++ right_usages ss'.
Proof. unfold right_usages; apply map_app. Qed.

Lemma left_usages_length (ss : splitting) :
  length (left_usages ss) = length ss.
Proof. unfold left_usages; apply map_length. Qed.

Lemma right_usages_length (ss : splitting) :
  length (right_usages ss) = length ss.
Proof. unfold right_usages; apply map_length. Qed.

Lemma used_splits_right {A : Type} {ss : splitting} {xs ys zs : list A} :
  splits ss zs xs ys ->
  used (right_usages ss) ys zs.
Proof. intros; induction H; simpl; auto. Qed.

Hint Resolve used_splits_right.

Lemma splits_keep_rights_left_usages {A : Type} {ss : list side} {g g1 g2 : list A} :
  splits ss g g1 g2 ->
  splits (keep_rights (left_usages ss)) g g1 g.
Proof.
  intros; induction H; simpl; eauto.
Qed.

Lemma used_filter_keeps_right_usages {A : Type} {ss : list side} {g g1 g2 : list A} :
  splits ss g g1 g2 ->
  used (filter_keeps (right_usages ss)) g2 g2.
Proof.
  intros; induction H; simpl; auto.
Qed.

Lemma select_iff_used :
  forall {A : Type} (us : list usage) (xs ys : list A),
    (length us = length xs /\ select us xs = ys) <-> used us ys xs.
Proof.
  intros A us; induction us; intros xs ys;
    split; intros H; try (destruct H as [Hlen H]; subst).
  - destruct xs; inversion Hlen; simpl; auto.
  - inversion H; subst; auto.
  - destruct xs; inversion Hlen; simpl;
      destruct a; constructor; apply IHus; auto.
  - inversion H; subst; simpl;
      eenough (length us = length ys0 /\ select us ys0 = _)
        by (destruct H0 as [Hlen H0]; rewrite Hlen; rewrite H0; auto);
        apply IHus; assumption.
Qed.

Hint Rewrite @select_iff_used.

Lemma used_select :
  forall {A : Type} (us : list usage) (xs : list A),
    length us = length xs ->
    used us (select us xs) xs.
Proof. intros; rewrite <- select_iff_used; auto. Qed.

Hint Resolve used_select.

Lemma select_iff_splits :
  forall {A : Type} (ss : splitting) (zs xs ys : list A),
    (List.length ss = List.length zs /\
     select (left_usages ss) zs = xs /\
     select (right_usages ss) zs = ys) <-> splits ss zs xs ys.
Proof.
  intros A ss; induction ss; intros zs xs ys; split; intros H; try (destruct H as [Hlen [Hl Hr]]).
  - subst; simpl; destruct zs; inversion Hlen; auto.
  - inversion H; subst; auto.
  - destruct zs; inversion Hlen; destruct a; subst; constructor; apply IHss; auto.
  - inversion H; subst;
      eenough (length ss = length zs0 /\
               select (left_usages ss) zs0 = _ /\
               select (right_usages ss) zs0 = _)
      by (simpl; destruct H0 as [Hlen [Hl Hr]]; rewrite Hlen, Hl, Hr; auto);
      apply IHss; auto.
Qed.

Hint Rewrite @select_iff_splits.

Lemma splits_select :
  forall {A : Type} (ss : splitting) (zs : list A),
    length ss = length zs ->
    splits ss zs (select (left_usages ss) zs) (select (right_usages ss) zs).
Proof. intros; rewrite <- select_iff_splits; auto. Qed.

Hint Resolve splits_select.

Fixpoint assoc_splitting1 (outer inner : splitting) : splitting :=
  match (outer, inner) with
  | (Left  :: outer, Left  :: inner) => Left  :: assoc_splitting1 outer inner
  | (Left  :: outer, Right :: inner) => Right :: assoc_splitting1 outer inner
  | (Left  :: outer, Both  :: inner) => Both  :: assoc_splitting1 outer inner
  | (Right :: outer,          inner) => Right :: assoc_splitting1 outer inner
  | (Both  :: outer, Left  :: inner) => Both  :: assoc_splitting1 outer inner
  | (Both  :: outer, Right :: inner) => Right :: assoc_splitting1 outer inner
  | (Both  :: outer, Both  :: inner) => Both  :: assoc_splitting1 outer inner
  | ([], inner) => []
  | (outer, []) => []
  end.

Fixpoint assoc_splitting2 (outer inner : splitting) : splitting :=
  match (outer, inner) with
  | (Left  :: outer, Left  :: inner) =>          assoc_splitting2 outer inner
  | (Left  :: outer, Right :: inner) => Left  :: assoc_splitting2 outer inner
  | (Left  :: outer, Both  :: inner) => Left  :: assoc_splitting2 outer inner
  | (Right :: outer,          inner) => Right :: assoc_splitting2 outer inner
  | (Both  :: outer, Left  :: inner) => Right :: assoc_splitting2 outer inner
  | (Both  :: outer, Right :: inner) => Both  :: assoc_splitting2 outer inner
  | (Both  :: outer, Both  :: inner) => Both  :: assoc_splitting2 outer inner
  | ([], inner) => []
  | (outer, []) => []
  end.

Definition assoc_splitting (outer inner : splitting) : splitting * splitting :=
  (assoc_splitting1 outer inner, assoc_splitting2 outer inner).

Definition flip_side (s : side) : side :=
  match s with
  | Left => Right
  | Right => Left
  | Both => Both
  end.

Definition flip_splitting (ss : splitting) : splitting :=
  List.map flip_side ss.

Lemma assoc_splits :
  forall {outer inner : splitting},
  forall {A : Type} {d1 g d2 g1 g2 : list A},
    splits outer d1 g d2 ->
    splits inner g g1 g2 ->
    exists d',
      splits (fst (assoc_splitting outer inner)) d1 g1 d' /\
      splits (snd (assoc_splitting outer inner)) d' g2 d2.
Proof.
  intros outer; induction outer; intros inner A g g1 g2 d1 d2 Houter Hinner;
    inversion Houter; subst; inversion Hinner; subst;
      eauto;
      try (specialize (IHouter _ _ _ _ _ _ _ H4 H5));
      try (specialize (IHouter _ _ _ _ _ _ _ H4 Hinner));
      destruct IHouter as [x' [Houter' Hinner']]; simpl; eauto.
Qed.

Lemma flip_splits :
  forall {A : Type} {ss : splitting},
  forall {g g1 g2 : list A},
    splits ss g g1 g2 ->
    splits (flip_splitting ss) g g2 g1.
Proof. intros; induction H; simpl; auto. Qed.

Lemma splits_flip_select :
  forall {A : Type} (ss : splitting) (zs : list A),
    length ss = length zs ->
    splits (flip_splitting ss) zs (select (right_usages ss) zs) (select (left_usages ss) zs).
Proof.
  intros; apply flip_splits, splits_select; assumption.
Qed.

Hint Resolve splits_flip_select.


(* currently only used by uncertified OCaml code at
   src/passes/13-scoping/scoping.ml *)
Fixpoint union (ls rs : list usage) : splitting * list usage :=
  match (ls, rs) with
  | ([], []) =>
    ([], [])
  | (l :: ls, r :: rs) =>
    let (ss, us) := union ls rs in
    match (l, r) with
    | (Drop, Drop) => (ss, Drop :: us)
    | (Drop, Keep) => (Right :: ss, Keep :: us)
    | (Keep, Drop) => (Left :: ss, Keep :: us)
    | (Keep, Keep) => (Both :: ss, Keep :: us)
    end
  (* ill-typed cases: *)
  (* | ([], r :: rs) => *)
  (*   let (ss, us) := coproduct [] rs in *)
  (*   match r with *)
  (*   | Drop => (Left :: ss, Keep :: us) *)
  (*   | Keep => (Both :: ss, Keep :: us) *)
  (*   end *)
  (* | (l :: ls, []) => *)
  (*   let (ss, us) := coproduct ls [] in *)
  (*   match l with *)
  (*   | Drop => (Right :: ss, Keep :: us) *)
  (*   | Keep => (Both :: ss, Keep :: us) *)
  (*   end *)
  | _ => ([], [])
  end.
