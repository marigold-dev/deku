open Types

val is_tuple_lmap : 'a Types.label_map -> bool

val get_pair : 'a Types.label_map -> ('a * 'a) option

val tuple_of_record : 'a LMap.t -> (label * 'a) list
val list_of_record_or_tuple : 'a LMap.t -> 'a list
val kv_list_of_record_or_tuple : 'a LMap.t -> (label * 'a) list

val fold_pattern : ('a -> 'b pattern -> 'a) -> 'a -> 'b pattern -> 'a
val fold_pattern_list : ('a -> 'b pattern -> 'a) -> 'a -> 'b pattern list -> 'a
val map_pattern_t : ('a binder -> 'b binder) -> 'a pattern -> 'b pattern

val var_attribute : binder_attributes
val const_attribute : binder_attributes
val empty_attribute : binder_attributes
