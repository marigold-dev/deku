type ('key, 'value) t = {
  merge : 'value -> 'value -> 'value;
  map : ('key, 'value) PolyMap.t;
}

val alias : ?debug:(Format.formatter -> 'key * 'value -> unit) -> demoted_repr:'key -> new_repr:'key -> ('key, 'value) t -> ('key, 'value) t

val create : cmp:('key -> 'key -> int) -> merge:('value -> 'value -> 'value) -> ('key, 'value) t
(* We don't export empty, since elements can be removed via
   List.fold add (empty s) (List.filter … @@ elements s) *)
(* val empty : ('key, 'value) t -> ('key, 'value) t *)
val is_empty : ('key, 'value) t -> bool
val add : ?debug:(Format.formatter -> 'key * 'value -> unit) -> 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t
val add_opt : ?debug:(Format.formatter -> 'key * 'value -> unit) -> 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t option
val monotonic_update : 'key -> ('value option -> 'value) -> ('key, 'value) t -> ('key, 'value) t
val find_opt : 'key -> ('key, 'value) t -> 'value option
val find_default : 'key -> (unit -> 'value) -> ('key, 'value) t -> 'value * ('key, 'value) t
val has_key : 'key -> ('key, 'value) t -> bool
val bindings : ('key, 'value) t -> ('key * 'value) list
val pp : (Format.formatter -> 'key -> unit) -> (Format.formatter -> 'value -> unit) -> Format.formatter -> ('key, 'value) t -> unit
