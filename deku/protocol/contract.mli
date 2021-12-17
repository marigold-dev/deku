
type t

val equal : t -> t -> bool
val compare: t -> t -> int
val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
val to_yojson : t -> Yojson.Safe.t
val to_string : t -> string
val of_string : string -> t option
val get_contract_opt : 'a -> 'b
