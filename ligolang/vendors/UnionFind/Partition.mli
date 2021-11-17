(** This module offers the abstract data type of a partition of
    classes of equivalent items (Union & Find). *)

(** The items are of type [Item.t], that is, they have to obey
    a total order, but also they must be printable to ease
    debugging. The signature [Item] is the input signature of
    the functor {!Partition.Make}. *)
module type Item =
  sig
    (** Type of items *)
    type t

    (** Same convention as {!Pervasives.compare} *)
    val compare : t -> t -> int

    val to_string : t -> string
  end

(** The module signature [S] is the output signature of the functor
    {!Partition.Make}. *)
module type S =
  sig
    type item
    type partition
    type t = partition

    (** {1 Creation} *)

    (** The value [empty] is an empty partition. *)
    val empty : partition

    (** The value of [equiv i j p] is the partition [p] extended with
        the equivalence of items [i] and [j]. If both [i] and [j] are
        already known to be equivalent, then [equiv i j p == p]. *)
    val equiv : item -> item -> partition -> partition

    (** The value of [alias i j p] is the partition [p] extended with
        the fact that item [i] is an alias of item [j]. This is the
        same as [equiv i j p], except that it is guaranteed that the
        item [i] is not the representative of its equivalence class in
        [alias i j p]. *)
    val alias : item -> item -> partition -> partition

    (** {1 Projection} *)

    (** The value of the call [repr i p] is [j] if the item [i] is in
        the partition [p] and its representative is [j]. If [i] is not
        in [p], then the value is [i]. *)
    val repr : item -> partition -> item

    (** The value of the call [mem i p] is [Some j] if the item [i] is
        in the partition [p] and its representative is [j]. If [i] is
        not in [p], then the value is [None]. *)
    val mem : item -> partition -> item option

    (** The call [print p] is a value of type [Buffer.t] containing
        strings denoting the partition [p], based on
        [Ord.to_string]. *)
    val print : partition -> Buffer.t

    (** {1 Predicates} *)

    (** The value of [is_equiv i j p] is [true] if, and only if, the
        items [i] and [j] belong to the same equivalence class in the
        partition [p], that is, [i] and [j] have the same
        representative. In particular, if either [i] or [j] do not
        belong to [p], the value of [is_equiv i j p] is [false]. See
        [mem] above. *)
    val is_equiv : item -> item -> partition -> bool
  end

module Make (Ord : Item) : S with type item = Ord.t
