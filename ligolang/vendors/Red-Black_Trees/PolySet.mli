(* Polymorphic ordered sets

   This module does not provide a function to merge polymorphic
   sets. Use the functorial interface of the module [Set] of the OCaml
   standard library instead.

   No deletion is provided.
*)

type 'elt t
type 'elt set = 'elt t

(* The value of the call [create ~cmp] is an empty set with [cmp]
   being the comparison over the (future) keys.

   The value [empty] is identical to the value of the call [create
   ~cmp:Pervasives.compare].
 *)

val create : cmp:('elt -> 'elt -> int) -> 'elt t

val empty : 'elt t -> 'elt t

(* Emptiness *)

val is_empty : 'elt t -> bool

(* The value of the call [add elt set] is the union of the set [set]
   and the singleton set containing [elt]. If there is an element [y]
   in [set] such that [cmp y elt = true], where [cmp] is the
   comparison function of the set [set] (see [create]), then [y] is
   replaced by [elt]. *)

val add : ?debug:(Format.formatter -> 'elt -> unit) -> 'elt -> 'elt t -> 'elt t

val union : 'elt t -> 'elt t -> 'elt t

(* The value of the call [remove elt set] is a set containing all the
   elements of the set [set] without the element [elt]. *)

val remove : ?debug:(Format.formatter -> 'elt -> unit)  -> 'elt -> 'elt t -> 'elt t

(* The value of the call [find elt set] is the element [y] of set
   [set] such that [cmp y elt = true], where [cmp] is the comparison
   function of [set] (see [create]). If [elt] is not in [set], then
   the exception [Not_found] is raised. *)

val find : 'elt -> 'elt t -> 'elt

(* The call [find_opt elt set] is similar to [find elt set], except
   that [None] is returned instead of the [Not_found] exception being
   raised, otherwise it is an optional element. *)

val find_opt : 'elt -> 'elt t -> 'elt option

(* The value of the call [mem elt set] is [true] if there exists an
   element [y] of set [set] such that [cmp y elt = true], where [cmp]
   is the comparison function of [set] (see [create]). If [elt] is not
   in [set], then [false] is returned instead. *)

val mem : 'elt -> 'elt t -> bool

(* The value of the call [add_list element_list set] is a record of
   type ['a added]. The elements from the [element_list] are added to
   the [set] starting from the head of the list. The elements which
   are already part of the [set] at the point at which they are added
   are gathered in the [duplicates] list (and the [set] is not updated
   for these elements, i.e. it keeps the pre-existing version of the
   element). The elements which are not already members of the [set]
   are added to the [set], and gathered in the [added] list. *)
type 'a added = {set : 'a t; duplicates : 'a list; added : 'a list}
val add_list : 'a list -> 'a t -> 'a added

(* The value of the call [elements set] is the list of elements of the
   set [set] in increasing order (with respect to the total comparison
   function used to create the set). *)
val elements : 'elt t -> 'elt list

(* map_elements f s is a shorthand for List.map f (elements s) *)
val map_elements : ('elt -> 'b) -> 'elt t -> 'b list

(* The value of the call [get_compare set] is the comparison function
   used by the given set *)
val get_compare : 'elt t -> ('elt -> 'elt -> int)

(* The side-effect of evaluating the call [iter f set] is the
   successive side-effects of the calls [f elt], for all the elements
   [elt] of the set [set], sorted in increasing order (with respect to
   the total comparison function used to create the set). *)

val iter : ('elt -> unit) -> 'elt t -> unit

(* The value of the call [fold_inc f set ~init] is the result of
   iterating the function [f] on all the elements of the set [set] in
   increasing order (with respect to the total comparison function
   used to create the set), accumulating partial results from the
   initial value [init]. *)

val fold_inc : ('elt -> acc:'a -> 'a) -> 'elt t -> init:'a -> 'a

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
