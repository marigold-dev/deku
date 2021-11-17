type 'k t = { size : int; m : ('k, int) PolyMap.t; }
val create : cmp:('elt -> 'elt -> int) -> 'elt t
val get_compare : 'elt t -> ('elt -> 'elt -> int)
val add : 'elt -> 'elt t -> 'elt t
val add_list : 'elt list -> 'elt t -> 'elt t
val of_list : cmp:('elt -> 'elt -> int) -> 'elt list -> 'elt t
val remove : 'elt -> 'elt t -> 'elt t
val union : 'elt t -> 'elt t -> 'elt t
val is_empty : 'elt t -> bool
val pp :
  (Format.formatter -> 'elt -> unit) -> Format.formatter -> 'elt t -> unit
val elements : 'elt t -> 'elt list
(* map_elements f s is a shorthand for List.map f (elements s) *)
val map_elements : ('elt -> 'b) -> 'elt t -> 'b list
