(* Polymorphic ordered sets whose keys are items in a union-find. When
   two keys are aliased in the union-find, the alias function should
   be called (be sure not to swap the other_repr and new_repr
   arguments!).

   The other definitions are re-exported from PolySet, see PolySet.mli
   for documentation.*)

type 'elt t
type 'elt set = 'elt t

val alias : ?debug:(Format.formatter -> 'elt -> unit) -> demoted_repr:'elt -> new_repr:'elt -> 'elt t -> 'elt t

val create : cmp:('elt -> 'elt -> int) -> 'elt t
(* We don't export empty, since elements can be removed via
   add_list (List.filter … @@ elements s) (empty s) *)
(* val empty : 'elt t -> 'elt t *)
val is_empty : 'elt t -> bool
val add : ?debug:(Format.formatter -> 'elt -> unit) -> 'elt -> 'elt t -> 'elt t
(* No removal, should be monotonic aside from merges due to aliasing *)
(* val remove : 'elt -> 'elt t -> 'elt t *)
val find : 'elt -> 'elt t -> 'elt
val find_opt : 'elt -> 'elt t -> 'elt option
val mem : 'elt -> 'elt t -> bool
type 'a added = {set : 'a t; duplicates : 'a list; added : 'a list}
val add_list : 'a list -> 'a t -> 'a added
val elements : 'elt t -> 'elt list
(* We don't export compare, since elements can be removed via
   add_list (List.filter … @@ elements s) (create ~cmp:(get_compare s)) *)
(* val get_compare : 'elt t -> ('elt -> 'elt -> int) *)
val iter : ('elt -> unit) -> 'elt t -> unit
val fold_inc : ('elt -> acc:'a -> 'a) -> 'elt t -> init:'a -> 'a
