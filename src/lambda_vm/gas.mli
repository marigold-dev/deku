type t

val make : initial_gas:int -> t
val measure : unit -> t

val is_empty : t -> bool

val burn_constant : t -> unit
val burn_log2 : t -> cardinality:int -> unit

val current : t -> int
