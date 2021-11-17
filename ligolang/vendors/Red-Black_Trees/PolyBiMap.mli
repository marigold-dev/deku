(* Polymorphic maps

   This module does not provide a function to merge polymorphic
   maps. Use the functorial interface of the module [Map] of the OCaml
   standard library instead, at the cost of the polymorphism on the
   keys.

   Deletion does not balance the tree.
*)

type ('key, 'value) t
type ('key, 'value) map = ('key, 'value) t

(* The value of the call [create ~cmp] is an empty map with [cmp]
   being the comparison over the (future) keys.

   The value [empty] is identical to the value of the call [create
   ~cmp:Pervasives.compare].
*)

val create : cmp_key:('key -> 'key -> int) -> cmp_value:('value -> 'value -> int) -> ('key, 'value) t

(* (\* The value of the call [from_list ~cmp elts] is a [Some map] with
 *    [cmp] being the comparison over the keys. The map initially
 *    contains the bindings listed in [elts]. If the same (w.r.t. [cmp])
 *    key occurs twice [elts] then [None] is returned instead to indicate
 *    the error. *\)
 * val from_list : cmp:('key -> 'key -> int) -> ('key * 'value) list -> ('key, 'value) t option *)

val empty : ('key, 'value) t -> ('key, 'value) t

(* Emptiness *)

val is_empty : ('key, 'value) t -> bool

(* The value of the call [add key value map] is a map containing all
   the bindings of the map [map], extended by the binding of [key] to
   [value]. If there is a binding for [key] in [map], its value is
   lost (and replaced by [value]). *)

val add : 'key -> 'value -> ('key, 'value) t -> ('key, 'value) t

(* The value of the call [remove_key key map] is a map containing all
   the bindings of the map [map], except for the binding of [key]. *)

val remove_key : 'key -> ('key, 'value) t -> ('key, 'value) t

(* The value of the call [remove_value value map] is a map containing all
   the bindings of the map [map], except for the binding of [value]. *)

val remove_value : 'value -> ('key, 'value) t -> ('key, 'value) t

(* The value of the call [find_value key map] is the value associated to the
   [key] in the map [map]. If [key] is not bound in [map], the
   exception [Not_found] is raised. *)

val find_value : 'key -> ('key, 'value) t -> 'value

(* The value of the call [find_key key map] is the key associated to the
   [value] in the map [map]. If [balue] is not bound in [map], the
   exception [Not_found] is raised. *)

val find_key : 'value -> ('key, 'value) t -> 'key

(* The value of the call [find_value_opt key map] is [Some value] if the key
   [key] is bound to [value] in the map [map], and [None]
   otherwise. *)

val find_value_opt : 'key -> ('key, 'value) t -> 'value option

(* The value of the call [find_key_opt value map] is [Some key] if the value
   [value] is bound to [key] in the map [map], and [None]
   otherwise. *)

val find_key_opt : 'value -> ('key, 'value) t -> 'key option

(* (\* The value of the call [find_default key make_default_v map] is
 *    [value] if the key [key] is bound to [value] in the map [map], and
 *    [make_default_v ()] otherwise. In the first case, the
 *    [make_default_v] function is not executed *\)
 * 
 * val find_default : 'key -> (unit -> 'value) -> ('key, 'value) map -> 'value * ('key, 'value) map
 * 
 * (\* The value of the call [find_opt key map] is [true] if the key
 *    [key] is bound to some value in the map [map], and [None]
 *    otherwise. *\)
 * 
 * val has_key : 'key -> ('key, 'value) t -> bool *)

(* The value of the call [update key f map] is a map containing all
   the bindings of the map [map], extended by the binding of [key] to
   the value returned by [f], when [f maybe_value] returns
   [Some value]. On the other hand, when [f maybe_value] returns
   [None], the existing binding for [key] in [map] is removed from the
   map, if there is one. The argument [maybe_value] passed to [f] is
   [Some value] if the key [key] is bound to [value] in the map [map],
   and [None] otherwise. *) 
val update_from_key : 'key -> ('value option -> ('key * 'value) option) -> ('key, 'value) map -> ('key, 'value) map

(* 
 * (\* The value of the call [bindings map] is the association list
 *    containing the bindings of the map [map], sorted by increasing keys
 *    (with respect to the total comparison function used to create the
 *    map). *\)
 * 
 * (\* The value of the call [add_list kv_list map] is a record of type
 *    [('key, 'value) added]. The elements from the [kv_list] are added
 *    to the [map] starting from the head of the list. The elements for
 *    which the key is already present in the [map] at the point at which
 *    they are added are gathered in the [duplicates] list (and the [map]
 *    is not updated for these elements, i.e. it keeps the pre-existing
 *    version of the value associated to that key). The elements for
 *    which the key is not already present in the [map] are added to the
 *    [map], and gathered in the [added] list. *\)
 * type ('key, 'value) added = {map : ('key, 'value) t; duplicates : ('key * 'value) list; added : ('key * 'value) list}
 * val add_list : ('key * 'value) list -> ('key, 'value) t -> ('key, 'value) added *)

val bindings : ('key, 'value) t -> ('key * 'value) list

(* (\* The side-effect of evaluating the call [iter f map] is the
 *    successive side-effects of the calls [f key value], for all
 *    bindings [(key, value)] belonging to the map [map], sorted in
 *    increasing order of the keys (with respect to the total comparison
 *    function used to create the map). *\)
 * 
 * val iter : ('key -> 'value -> unit) -> ('key, 'value) t -> unit
 * 
 * (\* The call [fold_inc f map ~init] computes [(f k_n v_n ~acc:(... (f
 *    k_1 v_1 ~acc:init)...)], where [k_1], ..., [k_n] are the keys of
 *    all bindings in the map [map] in increasing order, and [v_1], ...,
 *    [v_n] are the associated values. *\)
 * 
 * val fold_inc : ('key -> 'value -> acc:'a -> 'a) -> ('key, 'value) t -> init:'a -> 'a *)
