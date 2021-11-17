(** This module offers the abstract data type of a partition of
    classes of equivalent items (Union & Find). *)

(** The items are of type 't, they have to obey a total order,
    but also they must be printable to ease debugging. *)

type 'item partition
type 'item t = 'item partition
type 'item repr
type 'item node

(** {1 Creation} *)

(** The value [empty] is an empty partition. *)
val empty : (Format.formatter -> 'a -> unit) -> ('a -> 'a -> int) -> 'a partition

(** The value of [equiv i j p] is the partition [p] extended with
        the equivalence of items [i] and [j]. If both [i] and [j] are
        already known to be equivalent, then [equiv i j p == p]. *)
type 'item changed_reprs = { demoted_repr : 'item; new_repr : 'item }
type 'item equiv_result = { partition : 'item partition; changed_reprs : 'item changed_reprs }
val equiv : ?debug:(Format.formatter -> 'item * 'item node -> unit) -> 'item -> 'item -> 'item partition -> 'item equiv_result

(** The value of [alias i j p] is the partition [p] extended with
        the fact that item [i] is an alias of item [j]. This is the
        same as [equiv i j p], except that it is guaranteed that the
        item [i] is not the representative of its equivalence class in
        [alias i j p]. *)
(* val alias : 'item -> 'item -> 'item partition -> 'item partition *)

(** {1 Projection} *)

(** The value of the call [repr i p] is [j] if the item [i] is in
        the partition [p] and its representative is [j]. If [i] is not
        in [p], then the value is [i]. *)
val repr : 'item -> 'item partition -> 'item

(** The value of the call [get_or_set i p] is [j, p] if the item [i] is
        in the partition [p] and its representative is [j]. If [i] is not
        in [p], then the value is [i, p'], where p' is the partition [p]
        extended with the fact that item [i] is a singleton partition. *)

val get_or_set : ?debug:(Format.formatter -> 'item * 'item node -> unit) -> 'item -> 'item t -> 'item * 'item t

(** The value of the call [mem i p] is [Some j] if the item [i] is
        in the partition [p] and its representative is [j]. If [i] is
        not in [p], then the value is [None]. *)
(* val mem : 'item -> 'item partition -> 'item option *)

(** The value of the call [elements p] is a list of the elements of p,
    ordered in ascending order *)
(* val elements : 'item partition -> 'item list *)

(** The value of the call [partitions p] is a list of the partitions
   of p. A partition is a list of elements. The elements and
   partitions are returned with a deterministic order (regardless of
   the way the aliases have been made, the same partition will always
   have the same order). *)
val partitions : 'item partition -> 'item list list

(** The value of the call [get_compare p] is the comparison function
    used by p *)
(* val get_compare : 'item partition -> ('item -> 'item -> int) *)


(** The call [print p] is a value of type [Buffer.t] containing
        strings denoting the partition [p], based on
        [Ord.to_string]. *)
(* val print : Format.formatter -> 'item partition -> unit *)

(** {1 Predicates} *)

(** The value of [is_equiv i j p] is [true] if, and only if, the
        items [i] and [j] belong to the same equivalence class in the
        partition [p], that is, [i] and [j] have the same
        representative. In particular, if either [i] or [j] do not
        belong to [p], the value of [is_equiv i j p] is [false]. See
        [mem] above. *)
(* val is_equiv : 'item -> 'item -> 'item partition -> bool *)

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
