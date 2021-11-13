Local Set Warnings "-implicit-core-hint-db".
Set Implicit Arguments.
From Coq Require Import String List.
Import ListNotations.

From ligo_coq Require Import micheline.

Section types.

Context {A : Set}.

Local Open Scope string_scope.

Inductive iter_class : node A string -> node A string -> Prop :=
| Iter_list {l a n} : iter_class a (Prim l "list" [a] n)
| Iter_set {l a n} : iter_class a (Prim l "set" [a] n)
| Iter_map {l1 l2 k v n1 n2} : iter_class (Prim l1 "pair" [k; v] n1) (Prim l2 "map" [k; v] n2)
.

Inductive map_class : node A string -> node A string -> node A string -> node A string -> Prop :=
| Map_list {l1 l2 a b n1 n2} : map_class a b (Prim l1 "list" [a] n1) (Prim l2 "list" [b] n2)
| Map_map {l1 l2 l3 k v r n1 n2 n3} : map_class (Prim l1 "pair" [k; v] n1) r (Prim l2 "map" [k; v] n2) (Prim l3 "map" [k; r] n3)
.

Hint Constructors iter_class.
Hint Constructors map_class.

End types.
