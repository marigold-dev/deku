Set Implicit Arguments.
From Coq Require Import List.
Import ListNotations.

Lemma snoc_list_ind :
  forall {A : Set} (P : list A -> Prop),
    P [] -> (forall x xs, P xs -> P (xs ++ [x])) ->
    forall xs, P xs.
Proof.
  intros A' P Pnil Psnoc xs.
  enough (P (rev (rev xs))). rewrite rev_involutive in H. assumption.
  apply rev_list_ind.
  - assumption.
  - enough (forall a l, P (rev (rev l)) -> P (rev (a :: rev l))).
    { intros a l Prl. specialize (H a (rev l)).
      rewrite rev_involutive in H. specialize (H Prl).
      rewrite rev_involutive in H. assumption. }
    intros a l Pl. rewrite rev_involutive in Pl.
    simpl. rewrite rev_involutive.
    apply Psnoc; assumption.
Qed.

Lemma length_snoc {A : Type} {xs : list A} {x : A} :
  length (xs ++ [x]) = S (length xs).
Proof. induction xs; simpl; auto. Qed.
